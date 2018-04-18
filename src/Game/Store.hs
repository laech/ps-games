{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Game.Store
  ( getGames
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Pipes.Parse as P
import qualified Pipes.Prelude as P

import Control.Exception
import Control.Monad
import Data.Aeson hiding (decode)
import Data.Aeson.Types
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text, strip)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Typeable
import Game
import Network.HTTP.Client
import Pipes
import Pipes.Aeson
import Pipes.HTTP
import System.Log.Logger

newtype StoreException =
  StoreException String
  deriving (Show, Typeable)

instance Exception StoreException

failed :: String -> IO a
failed = throwIO . StoreException

parseGame :: Day -> Object -> Parser Game
parseGame date obj = do
  sku <- strip <$> obj .: "id"
  attrs <- obj .: "attributes"
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

pageSize = 500

getPageURL :: Int -> String
getPageURL offset =
  "https://store.playstation.com/valkyrie-api/en/NZ/999/container/STORE-MSF75508-FULLGAMES?size=" ++
  show pageSize ++ "&bucket=games&start=" ++ show offset

getGames :: Manager -> Producer Game IO ()
getGames man = do
  today <- lift $ utctDay <$> getCurrentTime
  getGamesJSON 0 man >-> P.mapFoldable (parse today)
  where
    parse date = parseMaybe $ parseGame date

getGamesJSON :: Int -> Manager -> Producer Object IO ()
getGamesJSON offset man = do
  page <- lift $ getPageJSON offset man
  games :: [Object] <- either failed' pure $ parseEither (.: "included") page
  for (each games) yield
  unless (null games) $ getGamesJSON (offset + pageSize) man
  where
    failed' = lift . failed

getPageJSON :: Int -> Manager -> IO Object
getPageJSON offset man = do
  infoM "Game" $ "Getting games from offset " ++ show offset ++ "..."
  req <- parseUrlThrow $ getPageURL offset
  withHTTP req man $ \resp ->
    P.evalStateT decode (responseBody resp) >>= \case
      Nothing -> failed "no content"
      Just (Left e) -> failed $ show e
      Just (Right r) -> pure r
