{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
  ( Game(..)
  , Games
  , PriceChanges
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

type PriceChanges = Map Day (Maybe Int)

data Game = Game
  { sku       :: Text
  , name      :: Text
  , platforms :: [Text]
  , sales     :: PriceChanges
  , prices    :: PriceChanges
  } deriving (Show, Generic)

$(deriveJSON defaultOptions ''Game)

update :: Games -> [Game] -> Games
update = foldl (\games game -> Map.insertWith update' (sku game) game games)
  where
    update' :: Game -> Game -> Game
    update' new old =
      new
      { prices = updateChanges prices old new
      , sales = updateChanges sales old new
      }

    updateChanges :: (Game -> PriceChanges) -> Game -> Game -> PriceChanges
    updateChanges f a b =
      let prices = Map.toList $ Map.union (f a) (f b)
      in Map.fromList . map head . groupByPrice . sortByDay $ prices

    sortByDay = sortBy (on compare fst)
    groupByPrice = groupBy (on (==) snd)
