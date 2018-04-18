{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Map as Map
import qualified Pipes.Prelude as P

import Control.Exception
import Data.Foldable
import Game
import Game.Database
import Game.Store
import Network.HTTP.Client.TLS
import Pipes
import Pipes.Core
import System.Environment
import System.Exit
import System.IO.Error
import System.Log.Logger

main :: IO ()
main =
  setupLogger *> getArgs >>= \case
    [dbFile] -> process dbFile
    _ -> die "Usage: <this-program> <db-json>"
  where
    setupLogger = updateGlobalLogger rootLoggerName (setLevel INFO)

process :: FilePath -> IO ()
process dbFile = update <$> getDb <*> getGames' >>= writeDb dbFile
  where
    getGames' = P.toListM $ getGames =<< newTlsManager
    getDb = catchJust doesNotExit (readDb dbFile) (const emptyDb)
    doesNotExit e =
      if isDoesNotExistError e
        then Just ()
        else Nothing
    emptyDb =
      Map.empty <$ putStrLn "File does not exists, using empty database."
