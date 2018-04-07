{-# LANGUAGE OverloadedStrings #-}

module Game.Store
  ( games
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Loops
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text, strip)
import Data.Time.Calendar
import Data.Time.Clock
import Game
import Pipes
import Pipes.Core
import Network.HTTP.Client
import System.Log.Logger

parseGames :: Day -> Value -> Parser [Game]
parseGames date =
  withObject "Games" $ \v ->
    mapMaybe (parseMaybe (parseGame date) . Object) <$> (v .: "included")

parseGame :: Day -> Value -> Parser Game
parseGame date =
  withObject "Game" $ \v -> do
    id <- strip <$> v .: "id"
    attrs <- v .: "attributes"
    name <- strip <$> attrs .: "name"
    releaseDate <- utctDay <$> (attrs .: "release-date")
    platforms <- sort . map strip <$> attrs .: "platforms"
    prices <- attrs .: "default-sku-id" >>= parsePrices attrs
    return
      Game
      { Game.id = id
      , name = name
      , releaseDate = releaseDate
      , platforms = platforms
      , history = Map.singleton date prices
      }

parsePrices :: Object -> Text -> Parser Prices
parsePrices attrs sku = do
  skus <- attrs .: "skus" :: Parser [Object]
  prices <-
    maybe (fail "No matching sku.") pure (find (matches sku) skus) >>=
    (.: "prices") >>=
    (.: "non-plus-user")
  actual <- parsePrice "actual-price" prices
  upsell <- parsePrice "upsell-price" prices
  strikethrough <- parsePrice "strikethrough-price" prices
  return
    Prices {actual = actual, upsell = upsell, strikethrough = strikethrough}
  where
    matches id o = HM.lookup "id" o == Just (String id)
    parsePrice tag prices = prices .:? tag >>= maybe (pure Nothing) (.: "value")

url :: Int -> String
url start =
  "https://store.playstation.com/valkyrie-api/en/NZ/999/container/STORE-MSF75508-FULLGAMES?size=500&bucket=games&start=" ++
  show start

games :: Manager -> Producer Game IO ()
games = games' 0

games' :: Int -> Manager -> Producer Game IO ()
games' offset manager = do
  today <- utctDay <$> lift getCurrentTime
  content <- lift $ readContent manager offset
  items <- either fail pure (eitherDecode content >>= parseEither (parseGames today))
  unless (null items) $ each items //> yield >> games' (offset + length items) manager

readContent manager offset = do
  infoM "Game" ("Downloading game data from offset " ++ show offset ++ "...")
  request <- parseUrlThrow $ url offset
  response <- httpLbs request manager
  pure $ responseBody response
