module Main where

import qualified Data.Map as Map

import Control.Exception
import Game
import Game.Database
import Game.Store
import Network.HTTP.Client.TLS
import System.Environment
import System.Exit
import System.IO.Error

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dbFile] -> process dbFile
    _ -> die "Usage: <this-program> <db-json>"

process :: FilePath -> IO ()
process dbFile = do
  db <- catchJust doesNotExit (readDb dbFile) (const emptyDb)
  putStrLn "Downloading game data..."
  games <- downloadGames =<< newTlsManager
  putStrLn $ "Downloaded data for " ++ show (length games) ++ " games."
  writeDb dbFile (update db games)
  where
    doesNotExit e =
      if isDoesNotExistError e
        then Just ()
        else Nothing
    emptyDb = do
      putStrLn "File does not exists, using empty database."
      return Map.empty