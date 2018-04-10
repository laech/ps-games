{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
readDb db = do
  content <- L.readFile db
  either fail (pure . process) (eitherDecode content)
  where
    process = Map.fromListWith merge . fmap (Game.id &&& Prelude.id)

writeDb :: FilePath -> Games -> IO ()
writeDb dbFile db = do
  let dir = takeDirectory dbFile
  temp <- openTempFile dir dbFile
  write temp `finally` clean temp
  where
    json = encodePretty' conf games
    games = sortBy (comparing Game.id) $ Map.elems db
    conf =
      defConfig
      { confIndent = Spaces 2
      , confCompare =
          compareKeys
            (Map.fromList $
             zip
               [ "id"
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
    compareKeys orders x y =
      case (Map.lookup x orders, Map.lookup y orders) of
        (Nothing, Nothing) -> compare y x
        (Just x', Just y') -> compare x' y'
        (Just _, _) -> LT
        (_, Just _) -> GT
    write (path, handle) = do
      L.hPut handle json
      hClose handle
      renameFile path dbFile
    clean (path, handle) = do
      hClose handle
      exists <- doesFileExist path
      when exists $ removeFile path
