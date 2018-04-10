{-# LANGUAGE OverloadedStrings #-}

module Game.Database
  ( readDb
  , writeDb
  ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map

import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.List
import Data.Map
import Data.Ord
import Game
import System.Directory
import System.FilePath
import System.IO

readDb :: FilePath -> IO Games
readDb path = do
  content <- L.readFile path
  either fail (pure . process) (eitherDecode content)
  where
    process = Map.fromListWith merge . fmap (sku &&& id)

writeDb :: FilePath -> Games -> IO ()
writeDb path db = do
  let dir = takeDirectory path
  temp <- openTempFile dir path
  write temp `finally` clean temp
  where
    json = encodePretty' jsonConf games
    games = sortBy (comparing sku) $ Map.elems db
    write (path, handle) = do
      L.hPut handle json
      hClose handle
      renameFile path path
    clean (path, handle) = do
      hClose handle
      exists <- doesFileExist path
      when exists $ removeFile path

jsonConf =
  defConfig
  { confIndent = Spaces 2
  , confCompare =
      jsonConfCompare
        (Map.fromList $
         zip
           [ "sku"
           , "name"
           , "releaseDate"
           , "platforms"
           , "history"
           , "date"
           , "upsell"
           , "actual"
           , "strikethrough"
           ]
           [0 ..])
  }

-- sort everything else by string (mainly for the dates)
jsonConfCompare orders x y =
  case (Map.lookup x orders, Map.lookup y orders) of
    (Nothing, Nothing) -> compare y x
    (Just x', Just y') -> compare x' y'
    (Just _, _) -> LT
    (_, Just _) -> GT
