{-# LANGUAGE LambdaCase #-}

module Main where

import Game
import Game.Database
import Game.Store
import Network.HTTP.Client.TLS
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dbFile] -> process dbFile
    _ -> die "Usage: <this-program> <db-json>"

process :: FilePath -> IO ()
process dbFile = do
  db <-
    readDbIfExists dbFile >>= \case
      Right db -> return db
      Left err -> die err
  putStrLn "Downloading game data..."
  games <- downloadGames =<< newTlsManager
  putStrLn $ "Downloaded data for " ++ show (length games) ++ " games."
  writeDb dbFile (update db games)