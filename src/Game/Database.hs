{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Game.Database
  ( readDb
  , writeDb
  ) where

import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Data.Map as Map

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.List
import Data.Ord
import Game

readDb :: FilePath -> IO Games
readDb dbFile =
  (eitherDecode <$> LazyChar8.readFile dbFile) >>= \case
    Right games -> return $ Map.fromListWith merge . fmap (\x -> (Game.id x, x)) $ games
    Left err -> fail err

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