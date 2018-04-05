{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Game.Database
  ( readDb
  , writeDb
  ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.List
import Data.Ord
import Game
import System.Directory
import System.FilePath
import System.IO

readDb :: FilePath -> IO Games
readDb dbFile =
  (eitherDecode <$> L.readFile dbFile) >>= \case
    Right games -> return $ Map.fromListWith merge . fmap (\x -> (Game.id x, x)) $ games
    Left err -> fail err

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
          keyOrder
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
      }
    write (path, handle) = do
      L.hPut handle json
      hClose handle
      renameFile path dbFile
    clean (path, handle) = do
      hClose handle
      exists <- doesFileExist path
      when exists $ removeFile path
