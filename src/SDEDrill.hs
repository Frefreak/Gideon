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
import Control.Monad
import Data.Scientific
import Data.List

import Constant
import Types
import IDIndex
import XML


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

saveListToFile :: FilePath -> [T.Text] -> IO ()
saveListToFile fp txt = TIO.writeFile fp (T.intercalate "\n" . nub . sort $ txt)

-- generate "NPCTradeList.txt"
genNPCTradeList :: IO ()
genNPCTradeList = do
    fp <- ((</> "NPCTradeList.txt") <$> sdeExtractionPath)
    tls <- getNPCTradeNonBlueprintList
    saveListToFile fp tls

getNPCCorpTradeList :: CorpIDType -> IO [T.Text]
getNPCCorpTradeList corpID = do
    Just (trades :: Value) <- crpNPCCorporationTradesYaml >>= Y.decodeFile
    let tradeID = map (T.pack . show . coefficient) $ trades ^.. _Array .
                traverse . filtered (\o -> o ^?!  key "corporationID" . _Number
                == corpID) .  key "typeID" . _Number
    Just items <- getItems
    if null tradeID then return [] else
        return $ filter (not . isSuffixOf "Blueprint" . T.unpack) $
        map (searchItemByID items) (nub . sort $ tradeID)

genNPCCorpTradeList :: CorpIDType -> IO ()
genNPCCorpTradeList corpID = do
    fp <- ((</> "NPCCorpTradeList-" ++ showSci corpID ++ ".txt")
            <$> sdeExtractionPath)
    tls <- getNPCCorpTradeList corpID
    saveListToFile fp tls
