{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
  ( Game(..)
  , Games
  , Prices(..)
  , update
  , merge
  ) where

import qualified Data.Map as Map

import Data.Aeson.TH
import Data.Function
import Data.List
import Data.Map (Map)
import Data.Ord
import Data.Text (Text)
import Data.Time.Calendar
import GHC.Generics

type Games = Map Text Game

data Prices = Prices
  { actual :: Maybe Int
  , upsell :: Maybe Int
  , strikethrough :: Maybe Int
  } deriving (Show, Eq, Generic)

data Game = Game
  { sku :: Text
  , name :: Text
  , releaseDate :: Day
  , platforms :: [Text]
  , history :: Map Day Prices
  } deriving (Show, Generic)

$(deriveJSON defaultOptions ''Prices)

$(deriveJSON defaultOptions ''Game)

update :: Games -> [Game] -> Games
update = foldl' update
  where
    update games game = Map.insertWith merge (sku game) game games

merge :: Game -> Game -> Game
merge new old = new {history = merge' . Map.unions . map history $ [old, new]}
  where
    merge' :: Map Day Prices -> Map Day Prices
    merge' = Map.fromList . getMinDate . groupByPrice . Map.toList
    getMinDate = map $ minimumBy $ comparing fst
    groupByPrice = groupBy $ on (==) snd
