{-# LANGUAGE OverloadedStrings #-}

module Game.Store
  ( games
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text, strip)
import Data.Time.Calendar
import Data.Time.Clock
import Game
import Network.HTTP.Client
import Pipes
import System.Log.Logger

parseGames :: Day -> Value -> Parser [Game]
parseGames date =
  withObject "Games" $ \v ->
    mapMaybe (parseMaybe $ parseGame date) <$> v .: "included"

parseGame :: Day -> Value -> Parser Game
parseGame date =
  withObject "Game" $ \v -> do
    sku <- strip <$> v .: "id"
    attrs <- v .: "attributes"
    name <- strip <$> attrs .: "name"
    releaseDate <- utctDay <$> attrs .: "release-date"
    platforms <- sort . map strip <$> attrs .: "platforms"
    prices <- attrs .: "default-sku-id" >>= parsePrices attrs
    pure
      Game
      { sku = sku
      , name = name
      , releaseDate = releaseDate
      , platforms = platforms
      , history = Map.singleton date prices
      }

parsePrices :: Object -> Text -> Parser Prices
parsePrices attrs sku = do
  skus <- attrs .: "skus" :: Parser [Object]
  prices <- search skus sku
  actual <- parse prices "actual-price"
  upsell <- parse prices "upsell-price"
  strike <- parse prices "strikethrough-price"
  pure Prices {actual = actual, upsell = upsell, strikethrough = strike}
  where
    parse prices tag = prices .:? tag >>= maybe (pure Nothing) (.: "value")
    search skus sku =
      let found = maybe (fail "sku not found") pure $ find matching skus
          matching o = HM.lookup "id" o == Just (String sku)
      in found >>= (.: "prices") >>= (.: "non-plus-user")

url :: Int -> String
url offset =
  "https://store.playstation.com/valkyrie-api/en/NZ/999/container/STORE-MSF75508-FULLGAMES?size=500&bucket=games&start=" ++
  show offset

games :: Manager -> Producer Game IO ()
games = games' 0

games' :: Int -> Manager -> Producer Game IO ()
games' offset manager = do
  today <- utctDay <$> lift getCurrentTime
  content <- lift $ readContent manager offset
  items <- either fail pure (parse content today)
  unless (null items) $ for (each items) yield >> next items
  where
    parse content date = eitherDecode content >>= parseEither (parseGames date)
    next items = games' (offset + length items) manager

readContent manager offset = do
  infoM "Game" ("Downloading game data from offset " ++ show offset ++ "...")
  request <- parseUrlThrow $ url offset
  response <- httpLbs request manager
  pure $ responseBody response
