module Main where

import System.Environment
import MarketUtil
import Thera

main :: IO ()
main = do
    [action, target] <- getArgs
    case action of
        "report" -> case target of
            "orders" -> reportSellAndBuyOrderStatus
            "sigs" -> reportUnscannedSigID
            otherwise -> putStrLn "Unknown target"
        otherwise -> putStrLn "Unknown action"

