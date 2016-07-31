{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module SDEDrill where

import System.FilePath
import System.Directory
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Maybe
import Control.Lens
import Data.Aeson.Lens
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Data.Scientific
import Data.List
import qualified Data.Vector as V
import GHC.Generics
import Control.Concurrent.Async
import qualified Data.ByteString.Lazy.Char8 as LBS

import Constant
import Types
import IDIndex
import XML
import CREST
import Util
import Auth


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

data SolarSystemPair = SSP
    { sspID :: SolarSystemIDType
    , sspName :: T.Text
    } deriving (Generic, Show)

data RegionSystems = RS
    { rsID :: RegionIDType
    , rsName :: T.Text
    , rsSystems :: V.Vector SolarSystemPair
    } deriving (Generic, Show)

instance ToJSON SolarSystemPair
instance ToJSON RegionSystems

getSystemPairs :: (Gideon (V.Vector SolarSystemPair) -> IO (V.Vector a))
                -> (RegionIDType, T.Text)
                -> V.Vector (T.Text, SolarSystemIDType)
                -> Gideon (V.Vector a)
getSystemPairs wrap (rid, rname) ssL = do
    let findname sid sl = fst . fromJust . V.find (\t -> snd t == sid) $ sl
    cons <- V.fromList <$> getConstellationsOfRegion rid
    rss <- liftIO $ forConcurrently cons $ \(cid, _) -> wrap $ do
        syss <- V.fromList <$> getSolarSystemsOfConstellation cid
        return $ V.map (\(sid, _) -> SSP sid (findname sid ssL)) syss
    return $ V.concat $ V.toList rss

genAllSolarSystemsMap' :: Gideon (V.Vector RegionSystems)
genAllSolarSystemsMap' = do
    regionList <- V.fromList <$> getRegionList
    ssList <- V.fromList <$> getSolarSystemList
    au <- liftIO getAuthInfo
    let wrapper1 = executeWithAuth au
        wrapper2 = executeWithAuth au
    liftIO $ forPool 10 regionList $ \(rname, rid) -> wrapper1 $
        (RS rid rname <$> getSystemPairs wrapper2 (rid, rname) ssList)

-- this function generates a file containing the hierarchy of Region > SolarSystems
-- while ignoring the constellation betwwen them
genAllSolarSystemsMap :: IO ()
genAllSolarSystemsMap = do
    fp <- allSolarSystemsJson
    Right vrs <- execute genAllSolarSystemsMap'
    LBS.writeFile fp (encodePretty vrs)

