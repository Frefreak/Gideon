{-# LANGUAGE OverloadedStrings #-}
module CREST.Market where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.Wreq
import Control.Lens
import qualified Data.Text as T
import Data.Aeson.Lens
import Data.Scientific
import Control.Monad.Reader

import Types
import Util

marketTypesUrl :: String
marketTypesUrl = composeCRESTUrl "market/types/"

getMarketType :: String -> Gideon String
getMarketType tid = do
    opts <- asks authOpt
    r <- liftIO . getWith opts $ composeCRESTUrl $ "market/types/" ++ tid ++ "/"
    return . T.unpack $ r ^. responseBody . key "type" . key "href" . _String

getMarketBuyOrders' :: RegionIDType -> String -> Gideon LBS.ByteString
getMarketBuyOrders' rid tid = do
    opts <- asks authOpt
    href <- getMarketType tid
    r <- liftIO $
        getWith opts $ composeCRESTUrl $ "market/" ++ show (coefficient rid)
        ++ "/orders/buy/?type=" ++ href
    return $ r ^. responseBody

getMarketBuyOrders :: RegionIDType -> String -> Gideon [MarketOrder]
getMarketBuyOrders rid tid = do
    lbs <- getMarketBuyOrders' rid tid
    return $ lbs ^.. key "items" . _Array . traverse . to (\o ->
        MarketOrder (o ^. key "location" . key "id_str" . _String)
                    (round $ o ^?! key "volumeEntered" . _Number)
                    (round $ o ^?! key "volume" . _Number)
                    (o ^. key "type" . key "id_str" . _String)
                    (toRealFloat $ o ^?! key "price" . _Number)
                    (o ^. key "id_str" . _String)
                )

getMarketSellOrders' :: RegionIDType -> String -> Gideon LBS.ByteString
getMarketSellOrders' rid tid = do
    opts <- asks authOpt
    href <- getMarketType tid
    r <- liftIO $
        getWith opts $ composeCRESTUrl $ "market/" ++ show (coefficient rid)
        ++ "/orders/sell/?type=" ++ href
    return $ r ^. responseBody

getMarketSellOrders :: RegionIDType -> String -> Gideon [MarketOrder]
getMarketSellOrders rid tid = do
    lbs <- getMarketSellOrders' rid tid
    return $ lbs ^.. key "items" . _Array . traverse . to (\o ->
        MarketOrder (o ^. key "location" . key "id_str" . _String)
                    (round $ o ^?! key "volumeEntered" . _Number)
                    (round $ o ^?! key "volume" . _Number)
                    (o ^. key "type" . key "id_str" . _String)
                    (toRealFloat $ o ^?! key "price" . _Number)
                    (o ^. key "id_str" . _String)
                )
