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

type AppConfig = MonadReader Options

-- program
main :: IO ()
--main = (\a -> runExceptT $ runReaderT runProgram a) =<< parseCLI
main = runReaderT runProgram =<< parseCLI

runProgram :: ReaderT Options IO ()
--runProgram :: App ()
runProgram = getSource
  >>= handleCapitalization
  >>= handleExcitedness
  >>= liftIO . putStr

getSource :: ReaderT Options IO String
--getSource :: App String
getSource = B.bool (either id id <$> loadContents) (liftIO getContents) =<< asks oStdIn

handleCapitalization :: String -> ReaderT Options IO String
--handleCapitalization :: String -> App String
handleCapitalization s = B.bool s (map C.toUpper s) <$> asks oCapitalize

handleExcitedness :: String -> ReaderT Options IO String
--handleExcitedness :: String -> App String
handleExcitedness s = B.bool s ("ZOMG " ++ s) <$> asks oExcited

type App a = ReaderT Options (ExceptT String IO) a

loadContents :: ReaderT Options IO (Either String String)
--loadContents :: App (Either String String)
loadContents = liftIO . readFileOrDefault =<< asks oFileToRead
  where
    readFileOrDefault :: Maybe FilePath -> IO (Either String [Char])
    readFileOrDefault = maybe defaultResponse readFileFromOptions

    readFileFromOptions :: FilePath -> IO (Either String String)
    readFileFromOptions f = BF.first show <$> safeReadFile f
    defaultResponse = return $ Right "This is fun!"

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
