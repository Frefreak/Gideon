{-# LANGUAGE OverloadedStrings #-}
module MarketUtil where

import Control.Exception
import Data.List
import Data.Function (on)
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Concurrent.Async
import Control.Concurrent.MSem
import Data.Either
import Data.Maybe
import Text.Printf

import XML
import Auth
import CREST
import Types
import Constant
import SDEDrill
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
    myorders <- extractAllMyBuyOrders
    opts <- asks authOpt
    sem <- liftIO $ new 150
    liftIO $ fmap catMaybes $ forConcurrently myorders $ with sem . \order -> do
        let region = stationToRegion stas (read . T.unpack $ moStationID order)
        if region == 0 then return Nothing else do
            orders <- getMarketBuyOrders opts region (T.unpack $ moTypeID order)
            return . Just $ (order, getBestBuyOrder .
                filter (\o -> moStationID o == moStationID order) $ orders)

reportBuyOrderStatus :: IO ()
reportBuyOrderStatus = do
    putStrLn $ "Querying buy orders..."
    r' <- execute reportBuyOrderStatus'
    case r' of
        Left err -> throw err
        Right r -> do
            printf "You have %s outstanding %s orders\n"
                (greenString (show $ length r)) ("buy" :: String)
            if null r then return () else do
                let quickfind tns o = let tid = moTypeID o in fromMaybe tid $ lookup tid tns
                putStrLn "getting TypeID -> TypeName mappings..."
                Right typenames <- execute $ getTypeName (moTypeID . fst <$> r)
                let rSort = sortBy (compare `on` (quickfind typenames . fst)) r
                putStrLn $ whiteString (replicate 120 '-')
                putStrLn $ yellowString "Buy" ++ " Order Summary"
                forM_ rSort $ \(self, opti) -> do
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
    opts <- asks authOpt
    sem <- liftIO $ new 150
    liftIO $ fmap catMaybes $ forConcurrently myorders $ with sem . \order -> do
        let region = stationToRegion stas (read . T.unpack $ moStationID order)
        if region == 0 then return Nothing else do
            orders <- getMarketSellOrders opts region (T.unpack $ moTypeID order)
            return . Just $ (order, getBestSellOrder .
                filter (\o -> moStationID o == moStationID order) $ orders)

reportSellOrderStatus :: IO ()
reportSellOrderStatus = do
    putStrLn $ "Querying sell orders..."
    r' <- execute reportSellOrderStatus'
    case r' of
        Left err -> throw err
        Right r -> do
            if null r then return () else do
                printf "You have %s outstanding %s orders\n"
                    (greenString (show $ length r)) ("sell" :: String)
                let quickfind tns o = let tid = moTypeID o in fromMaybe tid $ lookup tid tns
                putStrLn "getting TypeID -> TypeName mappings..."
                Right typenames <- execute $ getTypeName (moTypeID . fst <$> r)
                let rSort = sortBy (compare `on` (quickfind typenames . fst)) r
                putStrLn $ whiteString (replicate 120 '-')
                putStrLn $ yellowString "Sell" ++ " Order Summary"
                forM_ rSort $ \(self, opti) -> do
                    if moOrderID self == moOrderID opti then
                        printf "%s @ %f -- %s (%d/%d)\n" (quickfind typenames self)
                            (moPrice self) (blueString "optimal") (moVolRemaining opti)
                            (moVolEntered opti) else if moPrice self <= moPrice opti
                                                then
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
