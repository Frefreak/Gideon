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

getMarketType :: String -> IO String
getMarketType tid = do
    r <- get $ composeCRESTUrl $ "market/types/" ++ tid ++ "/"
    return . T.unpack $ r ^. responseBody . key "type" . key "href" . _String

getMarketBuyOrders :: RegionIDType -> String -> IO [MarketOrder]
getMarketBuyOrders rid tid = do
    href <- getMarketType tid
    r <- get $ composeCRESTUrl $ "market/" ++ show (coefficient rid)
        ++ "/orders/buy/?type=" ++ href
    return $ r ^.. responseBody . key "items" . _Array . traverse . to (\o ->
        MarketOrder (o ^. key "location" . key "id_str" . _String)
                    (round $ o ^?! key "volumeEntered" . _Number)
                    (round $ o ^?! key "volume" . _Number)
                    (o ^. key "type" . key "id_str" . _String)
                    (toRealFloat $ o ^?! key "price" . _Number)
                    (o ^. key "id_str" . _String)
                )

getMarketSellOrders :: RegionIDType -> String -> IO [MarketOrder]
getMarketSellOrders rid tid = do
    href <- getMarketType tid
    r <- get $ composeCRESTUrl $ "market/" ++ show (coefficient rid)
        ++ "/orders/sell/?type=" ++ href
    return $ r ^.. responseBody . key "items" . _Array . traverse . to (\o ->
        MarketOrder (o ^. key "location" . key "id_str" . _String)
                    (round $ o ^?! key "volumeEntered" . _Number)
                    (round $ o ^?! key "volume" . _Number)
                    (o ^. key "type" . key "id_str" . _String)
                    (toRealFloat $ o ^?! key "price" . _Number)
                    (o ^. key "id_str" . _String)
                )

dodixieRegion :: RegionIDType
dodixieRegion = 10000032

getMarketRegionHistory :: RegionIDType -> Gideon LBS.ByteString
getMarketRegionHistory rid = do
    r <- liftIO $ get $ composeCRESTUrl $ "market/" ++ show (coefficient rid)
        ++ "/types/34/history/"
    return $ r ^. responseBody
