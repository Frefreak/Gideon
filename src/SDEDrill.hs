{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module SDEDrill where

import System.FilePath
import System.Directory
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Maybe
import Data.Either
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
import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light
import qualified Data.Yaml as Y
import Data.Text.Encoding (encodeUtf8)
import Data.Function
import Data.Char (toLower)
import qualified Data.HashMap.Lazy as HM

import Constant
import Types
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
instance FromJSON SolarSystemPair
instance FromJSON RegionSystems

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



type Item = Value
type Items = HM.HashMap T.Text Value
type Stations = V.Vector Value

itemValid :: Item -> [Regex] -> Bool
itemValid it regs = and $ map containWord regs where
    desc = it ^. key "description" . key "en" . _String
    name = it ^. key "name" . key "en" . _String
    containWord wd = (desc =~ wd) || (name =~ wd)

filterItems :: Items -> [Regex] -> Items
filterItems items regs = HM.filter (flip itemValid regs) items

searchItems' :: Items -> [T.Text] -> HM.HashMap T.Text Value
searchItems' items inputs =
    let regs = rights $ map (flip compileM [caseless] . encodeUtf8) inputs
    in if length regs /= length inputs then
        HM.singleton "0" "" else
        filterItems items regs

searchItems :: Items -> [T.Text] -> HM.HashMap T.Text T.Text
searchItems items inputs = HM.map
    (\o -> o ^. key "name" . key "en" . _String) $ searchItems' items inputs

getItems :: IO (Maybe Items)
getItems = typeIDsYaml >>= Y.decodeFile

typeIDsYaml :: IO FilePath
typeIDsYaml = (</> "fsd/typeIDs.yaml") <$> databasePath

searchItemByID' :: Items -> T.Text -> Value
searchItemByID' items tid = items HM.! tid

searchItemByID :: Items -> T.Text -> T.Text
searchItemByID items tid = let item = items HM.! tid
    in item ^. key "name" . key "en" . _String

staStationsYaml :: IO FilePath
staStationsYaml = (</> "bsd/staStations.yaml") <$> databasePath

getStations :: IO (Maybe Stations)
getStations = staStationsYaml >>= Y.decodeFile >>=
    \(v :: Maybe Value) -> return (v ^? _Just . _Array)

stationToRegion :: Stations -> StationIDType -> RegionIDType
stationToRegion stas sid =
    let ls = V.filter (\o -> o ^?! key "stationID" . _Number == sid) stas
    in if null ls then 0 else (V.head ls) ^?! key "regionID" . _Number

allSolarSystemsJson :: IO FilePath
allSolarSystemsJson = (</> "allSolarSystems.json") <$> sdeExtractionPath

getAllSystems :: FilePath -> IO [T.Text]
getAllSystems fp = do
    lbs <- LBS.readFile fp
    let Just val = decode lbs :: Maybe Value
    return $ val ^.. _Array . traverse . key "rsSystems" . _Array . traverse . key "sspName" . _String

completeSolarSystemName :: T.Text -> IO [T.Text]
completeSolarSystemName prefix = do
    exist <- allSolarSystemsJson >>= doesFileExist
    if not exist then error "Run gen AllSystemsMap first!" else do
        solars <- allSolarSystemsJson >>= getAllSystems
        return $ filter (on T.isPrefixOf (T.map toLower) prefix) solars

