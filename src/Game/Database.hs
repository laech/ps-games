{-# LANGUAGE FlexibleContexts #-}
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
readDb path = L.readFile path >>= parse
  where
    parse content = either fail (pure . process) $ eitherDecode content
    process = Map.fromListWith merge . fmap (sku &&& id)

writeDb :: FilePath -> Games -> IO ()
writeDb path db = bracket (openTempFile dir path) clean (write path)
  where
    dir = takeDirectory path
    json = encodePretty' jsonConf games
    games = sortBy (comparing sku) $ Map.elems db
    write dst (path, handle) =
      L.hPut handle json *> hClose handle *> renameFile path dst
    clean (path, handle) =
      hClose handle *> doesFileExist path >>= \exists ->
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
