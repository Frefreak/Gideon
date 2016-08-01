{-# LANGUAGE OverloadedStrings #-}
module XML.Character where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.Wreq
import Control.Monad.Reader
import Control.Lens.Operators
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor

import Types
import Util

marketOrdersUrl :: Params -> String
marketOrdersUrl = composeXMLUrl "char/MarketOrders.xml.aspx"

getMyMarketOrders :: Gideon LBS.ByteString
getMyMarketOrders = do
    opt <- asks authOpt
    uid <- asks userID
    atk <- asks accessToken
    let newopt = opt & param "characterID" .~ [T.pack uid]
                    & param "accessToken" .~ [T.pack atk]
    r <- liftIO $ getWith newopt (marketOrdersUrl [])
    return $ r ^. responseBody

extractMyOrders :: T.Text -> LBS.ByteString -> [MarketOrder]
extractMyOrders bid lbs =
    let Right doc = parseLBS def lbs
        cursor = fromDocument doc
    in cursor $/ element "result" &/ element "rowset" &/ element "row"
        >=> attributeIs "orderState" "0" >=> attributeIs "bid" bid >=> \c ->
        MarketOrder <$> attribute "stationID" c <*>
                        (read . T.unpack <$> attribute "volEntered" c) <*>
                        (read . T.unpack <$> attribute "volRemaining" c) <*>
                        attribute "typeID" c <*>
                        (read . T.unpack <$> attribute "price" c) <*>
                        attribute "orderID" c

extractAllMyBuyOrders :: Gideon [MarketOrder]
extractAllMyBuyOrders = extractMyOrders "1" <$> getMyMarketOrders

extractAllMySellOrders :: Gideon [MarketOrder]
extractAllMySellOrders = extractMyOrders "0" <$> getMyMarketOrders

industryJobsUrl :: Params -> String
industryJobsUrl = composeXMLUrl "char/IndustryJobs.xml.aspx"


getIndustryJobs :: Gideon LBS.ByteString
getIndustryJobs = do
    opt <- asks authOpt
    uid <- asks userID
    atk <- asks accessToken
    let newopt = opt & param "characterID" .~ [T.pack uid]
                    & param "accessToken" .~ [T.pack atk]
    r <- liftIO $ getWith newopt (industryJobsUrl [])
    return $ r ^. responseBody
