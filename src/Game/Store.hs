{-# LANGUAGE OverloadedStrings #-}

module Game.Store
  ( downloadGames
  ) where

import           Control.Monad.Loops
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Text           (Text)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Game
import           Network.HTTP.Client

parseGames :: Day -> Value -> Parser [Game]
parseGames day =
  withObject "Games" $ \v ->
    mapMaybe (parseMaybe (parseGame day) . Object) <$> (v .: "included")

parseGame :: Day -> Value -> Parser Game
parseGame day =
  withObject "Game" $ \v -> do
    attrs <- v .: "attributes"
    sku <- attrs .: "default-sku-id"
    name <- attrs .: "name"
    platforms <- attrs .: "platforms"
    prices <- parsePrices day sku attrs
    return
      Game {sku = sku, name = name, platforms = platforms, history = [prices]}

parsePrices :: Day -> Text -> Object -> Parser Prices
parsePrices day sku attrs = do
  skus <- attrs .: "skus" :: Parser [Object]
  prices <-
    maybe (fail "No matching sku.") pure (find (matches sku) skus) >>=
    (.: "prices") >>=
    (.: "non-plus-user")
  actual <- parsePrice "actual-price" prices
  upsell <- parsePrice "upsell-price" prices
  strikethrough <- parsePrice "strikethrough-price" prices
  return
    Prices
    {day = day, actual = actual, upsell = upsell, strikethrough = strikethrough}
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
  today <- utctDay <$> getCurrentTime
  request <- parseUrlThrow $ gamesUrlAtStart start
  response <- httpLbs request manager
  let body = responseBody response
  case eitherDecode body >>= parseEither (parseGames today) of
    Right []    -> return $ Just ([], -1)
    Right games -> return $ Just (games, start + length games)
    Left err    -> fail err
