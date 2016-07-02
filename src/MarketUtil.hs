{-# LANGUAGE OverloadedStrings #-}
module MarketUtil where

import Data.List
import Data.Function (on)
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Concurrent.Async
import Data.Either
import Data.Maybe
import Text.Printf

import XML
import Auth
import CREST
import Types
import Constant
import IDIndex
import Terminal

getOptimalOrder :: (MarketOrder -> MarketOrder -> Ordering) -> [MarketOrder] -> MarketOrder
getOptimalOrder cmp = head . sortBy cmp

getBestSellOrder :: [MarketOrder] -> MarketOrder
getBestSellOrder = getOptimalOrder (\a b -> compare (moPrice a) (moPrice b))

getBestBuyOrder :: [MarketOrder] -> MarketOrder
getBestBuyOrder = getOptimalOrder (\a b -> compare (moPrice b) (moPrice a))

reportBuyOrderStatus' :: Gideon [(MarketOrder, MarketOrder)]
reportBuyOrderStatus' = do
    Just stas <- liftIO getStations
    myorders <- extractAllBuyOrders
    liftIO $ forConcurrently myorders $ \order -> do
        let region = stationToRegion stas (read . T.unpack $ moStationID order)
        orders <- liftIO $ getMarketBuyOrders region (T.unpack $ moTypeID order)
        return (order, getBestBuyOrder .
            filter (\o -> moStationID o == moStationID order) $ orders)

reportBuyOrderStatus :: IO ()
reportBuyOrderStatus = do
    putStrLn $ "Querying buy orders..."
    Right r <- execute reportBuyOrderStatus'
    printf "You have %s outstanding %s orders\n"
        (greenString (show $ length r)) ("buy" :: String)
    let quickfind tns o = let tid = moTypeID o in fromMaybe tid $ lookup tid tns
    putStrLn "getting TypeID -> TypeName mappings..."
    Right typenames <- execute $ getTypeName (moTypeID . fst <$> r)
    putStrLn $ yellowString "Buy" ++ " Order Summary"
    forM_ r $ \(self, opti) -> do
        if moOrderID self == moOrderID opti then
            printf "%s @ %f -- %s (%d/%d)\n"
                (quickfind typenames self)
                (moPrice self) (blueString "optimal") (moVolRemaining opti)
                (moVolEntered opti) else
            printf "%s @ %s -- %s, currently the best order is @ %s (%d/%d)\n"
                (cyanString . T.unpack $ quickfind typenames self)
                (greenString . show $ moPrice self) (blinkString "not optimal")
                (redString . show $ moPrice opti) (moVolRemaining self)
                (moVolEntered self)


reportSellOrderStatus' :: Gideon [(MarketOrder, MarketOrder)]
reportSellOrderStatus' = do
    Just stas <- liftIO getStations
    myorders <- extractAllSellOrders
    liftIO $ forConcurrently myorders $ \order -> do
        let region = stationToRegion stas (read . T.unpack $ moStationID order)
        orders <- liftIO $ getMarketSellOrders region (T.unpack $ moTypeID order)
        return (order, getBestSellOrder .
            filter (\o -> moStationID o == moStationID order) $ orders)

reportSellOrderStatus :: IO ()
reportSellOrderStatus = do
    putStrLn $ "Querying sell orders..."
    Right r <- execute reportSellOrderStatus'
    printf "\nYou have %s outstanding %s orders\n"
        (greenString (show $ length r)) ("sell" :: String)
    let quickfind tns o = let tid = moTypeID o in fromMaybe tid $ lookup tid tns
    putStrLn "getting TypeID -> TypeName mappings..."
    Right typenames <- execute $ getTypeName (moTypeID . fst <$> r)
    putStrLn $ yellowString "Sell" ++ " Order Summary"
    forM_ r $ \(self, opti) -> do
        if moOrderID self == moOrderID opti then
            printf "%s @ %f -- %s (%d/%d)\n" (quickfind typenames self)
                (moPrice self) (blueString "optimal") (moVolRemaining opti)
                (moVolEntered opti) else
            printf "%s @ %s -- %s, currently the best order is @ %s (%d/%d)\n"
                (cyanString . T.unpack $ quickfind typenames self)
                (redString . show $ moPrice self) (blinkString "not optimal")
                (greenString . show $ moPrice opti) (moVolRemaining self)
                (moVolEntered self)

reportSellAndBuyOrderStatus :: IO ()
reportSellAndBuyOrderStatus = do
    reportBuyOrderStatus
    putStrLn ""
    reportSellOrderStatus
