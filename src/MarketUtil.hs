{-# LANGUAGE OverloadedStrings #-}
module MarketUtil where

import Control.Exception
import Data.List
import Data.Function (on)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Maybe
import Text.Printf

import XML
import Auth
import CREST
import Types
import SDEDrill
import Terminal
import Util

getOptimalOrder :: (MarketOrder -> MarketOrder -> Ordering) -> [MarketOrder] -> MarketOrder
getOptimalOrder = minimumBy

marketOrderSellCompare :: MarketOrder -> MarketOrder -> Ordering
marketOrderSellCompare = \a b -> compare (moPrice a) (moPrice b)

marketOrderBuyCompare :: MarketOrder -> MarketOrder -> Ordering
marketOrderBuyCompare = \a b -> compare (moPrice b) (moPrice a)

getBestSellOrder :: [MarketOrder] -> MarketOrder
getBestSellOrder = getOptimalOrder marketOrderSellCompare

getBestBuyOrder :: [MarketOrder] -> MarketOrder
getBestBuyOrder = getOptimalOrder marketOrderBuyCompare

quickfind :: [(T.Text, T.Text)] -> MarketOrder -> T.Text
quickfind tns o = let tid = moTypeID o in fromMaybe tid $ lookup tid tns

reportBuyOrderStatus' :: Gideon [(MarketOrder, MarketOrder)]
reportBuyOrderStatus' = do
    Just stas <- liftIO getStations
    myorders <- extractAllMyBuyOrders
    wrapper <- executeWithAuth <$> ask
    liftIO $ fmap catMaybes $ forPool 150 myorders $ \order -> wrapper $ do
        let region = stationToRegion stas (read . T.unpack $ moStationID order)
        if region == 0 then return $ Just (order, order) else do
            orders <- getMarketBuyOrders region (T.unpack $ moTypeID order)
            return . Just $ (order, getBestBuyOrder .
                filter (\o -> moStationID o == moStationID order) $ orders)

reportBuyOrderStatus :: IO ()
reportBuyOrderStatus = do
    putStrLn "Querying buy orders..."
    r' <- execute reportBuyOrderStatus'
    case r' of
        Left err -> throw err
        Right r -> do
            printf "You have %s outstanding %s orders\n"
                (greenString (show $ length r)) ("buy" :: String)
            unless (null r) $ do
                Right typenames <- execute $ getTypeName (moTypeID . fst <$> r)
                let rSort = sortBy (compare `on` (quickfind typenames . fst)) r
                putStrLn $ whiteString (replicate 120 '-')
                putStrLn $ yellowString "Buy" ++ " Order Summary"
                forM_ rSort $ \(self, opti) ->
                    if moOrderID self == moOrderID opti then
                        printf "%s @ %f -- %s (%d/%d)\n"
                            (quickfind typenames self)
                            (moPrice self) (blueString "optimal")
                            (moVolRemaining opti) (moVolEntered opti)
                            else if moPrice self >= moPrice opti then
                        printf "%s @ %f -- order seems already %s\n"
                            (quickfind typenames self)
                            (moPrice self) (magentaString "filled") else
                        printf
                            "%s @ %s -- %s, currently the best order is @ %s (%d/%d)\n"
                            (cyanString . T.unpack $ quickfind typenames self)
                            (greenString . show $ moPrice self)
                            (blinkString "not optimal")
                            (redString . show $ moPrice opti) (moVolRemaining self)
                            (moVolEntered self)
                putStrLn $ whiteString (replicate 120 '-')


reportSellOrderStatus' :: Gideon [(MarketOrder, MarketOrder)]
reportSellOrderStatus' = do
    Just stas <- liftIO getStations
    myorders <- extractAllMySellOrders
    wrapper <- executeWithAuth <$> ask
    liftIO $ fmap catMaybes $ forPool 150 myorders $ \order -> wrapper $ do
        let region = stationToRegion stas (read . T.unpack $ moStationID order)
        if region == 0 then return $ Just (order, order) else do
            orders <- getMarketSellOrders region (T.unpack $ moTypeID order)
            return . Just $ (order, getBestSellOrder .
                filter (\o -> moStationID o == moStationID order) $ orders)

reportSellOrderStatus :: IO ()
reportSellOrderStatus = do
    putStrLn "Querying sell orders..."
    r' <- execute reportSellOrderStatus'
    case r' of
        Left err -> throw err
        Right r ->
            unless (null r) $ do
                printf "You have %s outstanding %s orders\n"
                    (greenString (show $ length r)) ("sell" :: String)
                Right typenames <- execute $ getTypeName (moTypeID . fst <$> r)
                let rSort = sortBy (compare `on` (quickfind typenames . fst)) r
                putStrLn $ whiteString (replicate 120 '-')
                putStrLn $ yellowString "Sell" ++ " Order Summary"
                forM_ rSort $ \(self, opti) ->
                    if moOrderID self == moOrderID opti then
                        printf "%s @ %f -- %s (%d/%d)\n" (quickfind typenames self)
                            (moPrice self) (blueString "optimal") (moVolRemaining opti)
                            (moVolEntered opti) else if moPrice self <= moPrice opti then
                        printf "%s @ %f -- order seems already %s\n"
                            (quickfind typenames self)
                            (moPrice self) (magentaString "filled") else
                        printf "%s @ %s -- %s, currently the best order is @ %s (%d/%d)\n"
                            (cyanString . T.unpack $ quickfind typenames self)
                            (redString . show $ moPrice self) (blinkString "not optimal")
                            (greenString . show $ moPrice opti) (moVolRemaining self)
                            (moVolEntered self)
                putStrLn $ whiteString (replicate 120 '-')

reportSellAndBuyOrderStatus :: IO ()
reportSellAndBuyOrderStatus = do
    reportBuyOrderStatus
    putStrLn ""
    reportSellOrderStatus


lookupItemSellOrdersInSystem :: T.Text -> T.Text -> Gideon [MarketOrder]
lookupItemSellOrdersInSystem item sys = do
    allSys <- liftIO getAllSystems
    let sysName = sanitizeSystemName allSys sys
    (_, rid) <- liftIO $ systemNameToRegion sysName
    tid <- liftIO $ itemNameToID item
    orders <- getMarketSellOrders rid (T.unpack tid)
    return $ filter (\mo -> sysName `T.isPrefixOf` moStationName mo) orders

lookupItemSellOrdersInSystemSorted :: T.Text -> T.Text -> Gideon [MarketOrder]
lookupItemSellOrdersInSystemSorted item sys =
    sortBy marketOrderSellCompare <$> lookupItemSellOrdersInSystem item sys

-- TODO to be optimized
lookupItemSellOrdersInSystems :: T.Text -> [T.Text] -> Gideon [MarketOrder]
lookupItemSellOrdersInSystems item sys =
    concat <$> mapM (lookupItemSellOrdersInSystem item) sys

lookupItemSellOrdersInSystemsSorted ::
    T.Text -> [T.Text] -> Gideon [MarketOrder]
lookupItemSellOrdersInSystemsSorted item sys =
    sortBy marketOrderSellCompare <$> lookupItemSellOrdersInSystems item sys

lookupItemNBestSellOrderInSystem :: T.Text -> T.Text -> Int -> Gideon [MarketOrder]
lookupItemNBestSellOrderInSystem item sys n =
    take n <$> lookupItemSellOrdersInSystemSorted item sys
