{-# LANGUAGE OverloadedStrings #-}

module Game.Database
  ( readDbIfExists
  , writeDb
  ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import           Data.List
import qualified Data.Map                   as Map
import           Data.Ord
import           Game
import           System.Directory

readDbIfExists :: FilePath -> IO (Either String Games)
readDbIfExists dbFile = do
  exists <- doesPathExist dbFile
  if exists
    then read
    else return $ Right Map.empty
  where
    read = do
      file <- LazyChar8.readFile dbFile
      let games = eitherDecode file :: Either String [Game]
      return $ Map.fromList . fmap (\x -> (sku x, x)) <$> games

writeDb :: FilePath -> Games -> IO ()
writeDb dbFile db = LazyChar8.writeFile dbFile json
  where
    json = encodePretty' conf games
    games = sortBy (comparing $ Down . releaseDate) $ Map.elems db
    conf =
      defConfig
      { confIndent = Spaces 2
      , confCompare =
          keyOrder
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
      }
