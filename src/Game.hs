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
  { date :: Day
  , actual :: Maybe Int
  , upsell :: Maybe Int
  , strikethrough :: Maybe Int
  } deriving (Show, Eq, Generic)

data Game = Game
  { id :: Text
  , name :: Text
  , releaseDate :: Day
  , platforms :: [Text]
  , history :: [Prices]
  } deriving (Show, Generic)

$(deriveJSON defaultOptions ''Prices)

$(deriveJSON defaultOptions ''Game)

update :: Games -> [Game] -> Games
update = foldl (\games game -> Map.insertWith merge (Game.id game) game games)

merge :: Game -> Game -> Game
merge new old = new {history = updateHistory history old new}
  where
    updateHistory :: (Game -> [Prices]) -> Game -> Game -> [Prices]
    updateHistory f a b = mapMinDate . groupByPrice . sortByDay $ f a ++ f b
    mapMinDate = map (minimumBy $ comparing date)
    sortByDay = sortBy (comparing $ Down . date)
    groupByPrice = groupBy (on (==) prices)
    prices x = [actual x, upsell x, strikethrough x]