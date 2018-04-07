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
main = do
  updateGlobalLogger rootLoggerName $ setLevel INFO
  args <- getArgs
  case args of
    [dbFile] -> process dbFile
    _ -> die "Usage: <this-program> <db-json>"

process :: FilePath -> IO ()
process dbFile = do
  db <- catchJust doesNotExit (readDb dbFile) (const emptyDb)
  infoM "Game" "Downloading game data..."
  manager <- newTlsManager
  games <- P.toListM $ games manager
  infoM "Game" ("Downloaded data for " ++ show (length games) ++ " games.")
  writeDb dbFile (update db games)
  where
    doesNotExit e =
      if isDoesNotExistError e
        then Just ()
        else Nothing
    emptyDb = do
      putStrLn "File does not exists, using empty database."
      return Map.empty
