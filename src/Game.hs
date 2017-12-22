{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
  ( Game(..)
  , Games
  , Prices(..)
  , update
  ) where

import           Data.Aeson.TH
import           Data.Function
import           Data.List
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Text          (Text)
import           Data.Time.Calendar
import           GHC.Generics

type Games = Map Text Game

data Prices = Prices
  { day           :: Day
  , actual        :: Maybe Int
  , upsell        :: Maybe Int
  , strikethrough :: Maybe Int
  } deriving (Show, Eq, Generic)

data Game = Game
  { sku       :: Text
  , name      :: Text
  , platforms :: [Text]
  , history   :: [Prices]
  } deriving (Show, Generic)

$(deriveJSON defaultOptions ''Prices)

$(deriveJSON defaultOptions ''Game)

update :: Games -> [Game] -> Games
update =
  foldl
    (\games game ->
       Map.insertWith
         (\new old -> new {history = updateHistory history old new})
         (sku game)
         game
         games)
  where
    updateHistory :: (Game -> [Prices]) -> Game -> Game -> [Prices]
    updateHistory f a b = map head . groupByPrice . sortByDay $ f a ++ f b
    sortByDay = sortBy (on compare day)
    groupByPrice = groupBy (on (==) prices)
    prices x = [actual x, upsell x, strikethrough x]
