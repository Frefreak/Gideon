{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SDEDrill where

import System.FilePath
import Data.Aeson
import Control.Lens
import Data.Aeson.Lens
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)
import Data.Scientific
import Data.List

import Constant
import Types
import IDIndex


getNPCTradeItemList :: IO [T.Text]
getNPCTradeItemList = do
    Just (trades :: Value) <- crpNPCCorporationTradesYaml >>= Y.decodeFile
    let tradeID = map (T.pack . show . coefficient) $
            trades ^.. _Array . traverse . key "typeID" . _Number
    Just items <- getItems
    let allTrades = map (searchItemByID items) tradeID
    return . nub . sort $ allTrades

getNPCTradeNonBlueprintList :: IO [T.Text]
getNPCTradeNonBlueprintList =
    filter (not . T.isSuffixOf "Blueprint") <$> getNPCTradeItemList

crpNPCCorporationTradesYaml :: IO FilePath
crpNPCCorporationTradesYaml = (</> "bsd/crpNPCCorporationTrades.yaml") <$>
                                databasePath

-- generate "NPCTradeList.txt"
genNPCTradeList :: IO ()
genNPCTradeList = do
    file <- (</> "NPCTradeList.txt") <$> sdeExtractionPath
    res <- getNPCTradeNonBlueprintList
    TIO.writeFile file (T.intercalate "\n" . nub . sort $ res)

