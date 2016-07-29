{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SDEDrill where

import System.FilePath
import System.Directory
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
{-import Control.Concurrent.Async-}

import Constant
import Types
import IDIndex
import XML
import Util


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
    fp <- (</> "NPCTradeList.txt") <$> sdeExtractionPath
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

getRegionId :: FilePath -> IO T.Text
getRegionId fp = do
    Just (rinfo :: Value) <- Y.decodeFile fp
    return $ T.pack . show . coefficient $ rinfo ^?! key "regionID" . _Number

getSystemId :: FilePath -> IO T.Text
getSystemId fp = do
    Just (rinfo :: Value) <- Y.decodeFile fp
    return $ T.pack . show . coefficient $ rinfo ^?! key "solarSystemID" . _Number

-- takes a long time on my laptop :(
genAllSolarSystemsMap :: IO ()
genAllSolarSystemsMap = do
    fp <- allSolarSystemsTxt
    eve <- (</> "fsd/universe/eve") <$> databasePath
    folds <- listDirectory eve >>= return . map (eve </>)
    result <- concat <$> mapM intoRegion folds
    let text = T.unlines $ map (T.intercalate " ") result
    TIO.writeFile fp text where
        getD f sta = do
            files <- listDirectory f
            return (filterMap (/= sta) (f </>) files, f </> sta)
        intoRegion r = do
            (constellations, sta) <- getD r "region.staticdata"
            rid <- getRegionId sta
            tups <- concat <$> mapM intoConstellation constellations
            return $ map (\(sname, sid) -> [sname, sid, T.pack $ takeBaseName r, rid]) tups
        intoConstellation c = do
            (solars, _) <- getD c "constellation.staticdata"
            mapM intoSolar solars
        intoSolar s = do
            sid <- getSystemId $ s </> "solarsystem.staticdata"
            return (T.pack $ takeBaseName s, sid)


