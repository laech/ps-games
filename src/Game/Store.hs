{-# LANGUAGE OverloadedStrings #-}

module Game.Store
  ( downloadGames
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
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
import Game
import Network.HTTP.Client
import System.Log.Logger

parseGames :: Day -> Value -> Parser [Game]
parseGames date =
  withObject "Games" $ \v ->
    mapMaybe (parseMaybe (parseGame date) . Object) <$> (v .: "included")

parseGame :: Day -> Value -> Parser Game
parseGame date =
  withObject "Game" $ \v -> do
    id <- v .: "id"
    attrs <- v .: "attributes"
    name <- attrs .: "name"
    releaseDate <- utctDay <$> (attrs .: "release-date")
    platforms <- sort <$> attrs .: "platforms"
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

gamesUrlAtStart :: Int -> String
gamesUrlAtStart start =
  "https://store.playstation.com/valkyrie-api/en/NZ/999/container/STORE-MSF75508-FULLGAMES?size=500&bucket=games&start=" ++
  show start

downloadGames :: Manager -> IO [Game]
downloadGames manager = concat <$> unfoldrM (download manager) 0

download :: Manager -> Int -> IO (Maybe ([Game], Int))
download _ (-1) = return Nothing
download manager start = do
  infoM "Game" ("Downloading game data from offset " ++ show start ++ "...")
  today <- utctDay <$> getCurrentTime
  request <- parseUrlThrow $ gamesUrlAtStart start
  response <- httpLbs request manager
  let body = responseBody response
  case eitherDecode body >>= parseEither (parseGames today) of
    Right [] -> return $ Just ([], -1)
    Right games -> return $ Just (games, start + length games)
    Left err -> fail err