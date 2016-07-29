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
            otherwise -> putStrLn "Unknown target"
        "gen"       -> case target of
            "allSystemsMap" -> genAllSolarSystemsMap
        otherwise -> putStrLn "Unknown action"

