module Main where

import System.Environment
import MarketUtil
import Thera
import SDEDrill

main :: IO ()
main = do
    [action, target] <- getArgs
    case action of
        "report"    -> case target of
            "orders" -> reportSellAndBuyOrderStatus
            "sigs" -> reportUnscannedSigID
            _ -> putStrLn "Unknown target"
        "gen"       -> case target of
            "allSystemsMap" -> genAllSolarSystemsMap
            _ -> putStrLn "Unknown target"
        _           -> putStrLn "Unknown action"

