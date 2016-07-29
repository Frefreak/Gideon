{-# LANGUAGE OverloadedStrings #-}
module CREST.Market where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Reader
import Network.Wreq
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Aeson.Lens
import Data.Scientific

import Auth
import Constant
import Types

marketTypesUrl = composeCRESTUrl $ "market/types/"

getMarketType :: Options -> String -> IO String
getMarketType opts tid = do
    r <- getWith opts $ composeCRESTUrl $ "market/types/" ++ tid ++ "/"
    return . T.unpack $ r ^. responseBody . key "type" . key "href" . _String

getMarketBuyOrders' :: Options -> RegionIDType -> String -> IO LBS.ByteString
getMarketBuyOrders' opts rid tid = do
    href <- getMarketType opts tid
    r <- getWith opts $ composeCRESTUrl $ "market/" ++ show (coefficient rid)
        ++ "/orders/buy/?type=" ++ href
    return $ r ^. responseBody

getMarketBuyOrders :: Options -> RegionIDType -> String -> IO [MarketOrder]
getMarketBuyOrders opts rid tid = do
    lbs <- getMarketBuyOrders' opts rid tid
    return $ lbs ^.. key "items" . _Array . traverse . to (\o ->
        MarketOrder (o ^. key "location" . key "id_str" . _String)
                    (round $ o ^?! key "volumeEntered" . _Number)
                    (round $ o ^?! key "volume" . _Number)
                    (o ^. key "type" . key "id_str" . _String)
                    (toRealFloat $ o ^?! key "price" . _Number)
                    (o ^. key "id_str" . _String)
                )

getMarketSellOrders' :: Options -> RegionIDType -> String -> IO LBS.ByteString
getMarketSellOrders' opts rid tid = do
    href <- getMarketType opts tid
    r <- getWith opts $ composeCRESTUrl $ "market/" ++ show (coefficient rid)
        ++ "/orders/sell/?type=" ++ href
    return $ r ^. responseBody

getMarketSellOrders :: Options -> RegionIDType -> String -> IO [MarketOrder]
getMarketSellOrders opts rid tid = do
    lbs <- getMarketSellOrders' opts rid tid
    return $ lbs ^.. key "items" . _Array . traverse . to (\o ->
        MarketOrder (o ^. key "location" . key "id_str" . _String)
                    (round $ o ^?! key "volumeEntered" . _Number)
                    (round $ o ^?! key "volume" . _Number)
                    (o ^. key "type" . key "id_str" . _String)
                    (toRealFloat $ o ^?! key "price" . _Number)
                    (o ^. key "id_str" . _String)
                )
