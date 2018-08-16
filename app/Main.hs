{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Control.Exception    as E
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Bifunctor       as BF
import qualified Data.Bool            as B
import qualified Data.Char            as C
import           Options.Applicative

-- types

data Options = Options
    { oCapitalize :: Bool
    , oExcited    :: Bool
    , oStdIn      :: Bool
    , oFileToRead :: Maybe String
    }
data AppError = IOError E.IOException deriving Show
type App a = ReaderT Options (ExceptT AppError IO) a

-- program
main :: IO ()
main = runProgram =<< parseCLI
  where
    runProgram = (print =<<) . runExceptT . runReaderT runApp

runApp :: App ()
runApp = getSource
  >>= handleCapitalization
  >>= handleExcitedness
  >>= liftIO . putStr

getSource :: App String
getSource = B.bool loadContents (liftIO getContents) =<< asks oStdIn

handleCapitalization :: String -> App String
handleCapitalization s = B.bool s (map C.toUpper s) <$> asks oCapitalize

handleExcitedness :: String -> App String
handleExcitedness s = B.bool s ("ZOMG " ++ s) <$> asks oExcited


loadContents :: App String
loadContents =
    maybe defaultResponse readFileFromOptions =<< asks oFileToRead
  where
    readFileFromOptions :: FilePath -> App String
    readFileFromOptions f = either throwError return =<< BF.first IOError <$> liftIO (safeReadFile f)

    defaultResponse :: App String
    defaultResponse = return "This is fun!"

-- CLI parsing
parseCLI :: IO Options
parseCLI = execParser (withInfo parseOptions "File Fun")
  where
    withInfo opts h = info (helper <*> opts) $ header h

parseOptions :: Parser Options
parseOptions = Options
    <$> (switch $ long "capitalize")
    <*> (switch $ long "excited")
    <*> (switch $ long "stdin")
    <*> (optional $ strOption $ long "file")

-- safer reading of files
safeReadFile :: FilePath -> IO (Either E.IOException String)
safeReadFile = E.try . readFile
