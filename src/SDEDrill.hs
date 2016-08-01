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
import Data.Scientific
import Data.List
import qualified Data.Vector as V
import GHC.Generics hiding (to)
import Control.Concurrent.Async
import qualified Data.ByteString.Lazy.Char8 as LBS
import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light
import Data.Text.Encoding (encodeUtf8)
import Data.Function
import Data.Char (toLower)
import qualified Data.HashMap.Lazy as HM
import Control.Exception

import Constant
import Types
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
    fp <- (</>) ("NPCCorpTradeList-" ++ showSci corpID ++ ".txt")
            <$> sdeExtractionPath
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

type Item = Value
type Items = HM.HashMap T.Text Value
type Stations = V.Vector Value

itemValid :: Item -> [Regex] -> Bool
itemValid it = all containWord where
    desc = it ^. key "description" . key "en" . _String
    name = it ^. key "name" . key "en" . _String
    containWord wd = (desc =~ wd) || (name =~ wd)

filterItems :: Items -> [Regex] -> Items
filterItems items regs = HM.filter (`itemValid` regs) items

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
    in if null ls then 0 else V.head ls ^?! key "regionID" . _Number

-- this function generates a file containing the hierarchy of Region > SolarSystems
-- while ignoring the constellation betwwen them
genAllSolarSystemsMap :: IO ()
genAllSolarSystemsMap = do
    fp <- allSolarSystemsJson
    res <- execute genAllSolarSystemsMap'
    case res of
        Left err -> throwIO err
        Right vrs -> LBS.writeFile fp (encodePretty vrs)

allSolarSystemsJson :: IO FilePath
allSolarSystemsJson = (</> "allSolarSystems.json") <$> sdeExtractionPath

getAllSystems :: IO [T.Text]
getAllSystems = do
    exist <- allSolarSystemsJson >>= doesFileExist
    if not exist then error "Run gen AllSystemsMap first!" else do
        lbs <- allSolarSystemsJson >>= LBS.readFile
        let Just val = decode lbs :: Maybe Value
        return $ val ^.. _Array . traverse . key "rsSystems" .
            _Array . traverse . key "sspName" . _String

getSystemPairs :: (Gideon (V.Vector SolarSystemPair) -> IO (V.Vector a))
                -> (RegionIDType, T.Text)
                -> V.Vector (T.Text, SolarSystemIDType)
                -> Gideon (V.Vector a)
getSystemPairs wrap (rid, _) ssL = do
    let findname sid = fst . fromJust . V.find (\t -> snd t == sid)
    consts <- V.fromList <$> getConstellationsOfRegion rid
    rss <- liftIO $ forConcurrently consts $ \(cid, _) -> wrap $ do
        syss <- V.fromList <$> getSolarSystemsOfConstellation cid
        return $ V.map (\(sid, _) -> SSP sid (findname sid ssL)) syss
    return $ V.concat $ V.toList rss

genAllSolarSystemsMap' :: Gideon (V.Vector RegionSystems)
genAllSolarSystemsMap' = do
    regionList <- V.fromList <$> getRegionList
    ssList <- V.fromList <$> getSolarSystemList
    auth <- liftIO getAuthInfo
    let wrapper1 = executeWithAuth auth
        wrapper2 = executeWithAuth auth
    liftIO $ forPool 7 regionList $ \(rname, rid) -> wrapper1
        (RS rid rname <$> getSystemPairs wrapper2 (rid, rname) ssList)

-- TODO calculate string similarity and return the best result
completeSolarSystemName' :: [T.Text] -> T.Text -> [T.Text]
completeSolarSystemName' solars prefix = sortBy (compare `on` T.length) $
    filter (on T.isPrefixOf (T.map toLower) prefix) solars

completeSolarSystemName :: T.Text -> IO [T.Text]
completeSolarSystemName prefix = do
    sys <- getAllSystems
    return $ completeSolarSystemName' sys prefix

sanitizeSystemName :: [T.Text] -> T.Text -> T.Text
sanitizeSystemName solars t =
    let candidates = completeSolarSystemName' solars t
    in if null candidates then error $ "No such system name: " ++ T.unpack t
        else head candidates

sanitizeSystemNames :: [T.Text] -> [T.Text] -> [T.Text]
sanitizeSystemNames solars = map (sanitizeSystemName solars)

type SystemMapping =
    HM.HashMap (T.Text, RegionIDType) [(T.Text, SolarSystemIDType)]

getSystemMappings :: IO SystemMapping
getSystemMappings = do
    exist <- allSolarSystemsJson >>= doesFileExist
    if not exist then error "Run genAllSystemsMap first!" else do
        lbs <- allSolarSystemsJson >>= LBS.readFile
        return . HM.fromList $ lbs ^.. _Array . traverse . to (\o ->
            ((o ^. key "rsName" . _String,
            o ^?! key "rsID" . _Number), o ^.. key "rsSystems" . _Array .
                traverse . to (\o' -> (o' ^. key "sspName" . _String,
                                        o' ^?! key "sspID" . _Number))))

systemNameToRegion' :: SystemMapping -> T.Text -> (T.Text, RegionIDType)
systemNameToRegion' sm t =
    head . HM.keys $ HM.filter (\v -> t `elem` map fst v) sm

systemIDToRegion' :: SystemMapping -> SolarSystemIDType ->
    (T.Text, RegionIDType)
systemIDToRegion' sm ssit =
    head . HM.keys $ HM.filter (\v ->ssit `elem` map snd v) sm

systemNamesToRegion' :: SystemMapping -> [T.Text] -> [(T.Text, RegionIDType)]
systemNamesToRegion' sm = map (systemNameToRegion' sm)

systemIDsToRegion' :: SystemMapping -> [SolarSystemIDType]
    -> [(T.Text, RegionIDType)]
systemIDsToRegion' sm = map (systemIDToRegion' sm)

systemNameToRegion :: T.Text -> IO (T.Text, RegionIDType)
systemNameToRegion t = do
    sys <- getAllSystems
    let t' = sanitizeSystemName sys t
    fmap (`systemNameToRegion'` t') getSystemMappings

systemNamesToRegion :: [T.Text] -> IO [(T.Text, RegionIDType)]
systemNamesToRegion t = do
    sys <- getAllSystems
    let t' = sanitizeSystemNames sys t
    fmap (`systemNamesToRegion'` t') getSystemMappings

systemIDToRegion :: SolarSystemIDType -> IO (T.Text, RegionIDType)
systemIDToRegion ssit = fmap (`systemIDToRegion'` ssit) getSystemMappings

systemIDsToRegion :: [SolarSystemIDType] -> IO [(T.Text, RegionIDType)]
systemIDsToRegion ssit = fmap (`systemIDsToRegion'` ssit) getSystemMappings 

-- generate mapping between items name and type id
allItemsJson :: IO FilePath
allItemsJson = (</> "allItems.json") <$> sdeExtractionPath

type ItemMapping = HM.HashMap T.Text T.Text

genAllItemsMap :: IO ()
genAllItemsMap = do
    src <- typeIDsYaml
    dest <- allItemsJson
    Just hm <- Y.decodeFile src :: IO (Maybe (HM.HashMap T.Text Value))
    let val' = HM.map (\o -> o ^. key "name" . key "en" . _String) hm
    LBS.writeFile dest (encodePretty val')

getAllItemsName :: IO [T.Text]
getAllItemsName = do
    exist <- allItemsJson >>= doesFileExist
    if not exist then error "Run genAllItemsMap first!" else do
        Just hm <- decode <$> (LBS.readFile =<< allItemsJson)
            :: IO (Maybe (HM.HashMap T.Text T.Text))
        return $ HM.elems hm

-- TODO calculate string similarity and return the best result
completeItemName' :: [T.Text] -> T.Text -> [T.Text]
completeItemName' = completeSolarSystemName'

completeItemName :: T.Text -> IO [T.Text]
completeItemName prefix = do
    items <- getAllItemsName
    return $ completeItemName' items prefix

sanitizeItemName :: [T.Text] -> T.Text -> T.Text
sanitizeItemName items t =
    let candidates = completeItemName' items t
    in if null candidates then error $ "No such Item name: " ++ T.unpack t
        else head candidates

sanitizeItemNames :: [T.Text] -> [T.Text] -> [T.Text]
sanitizeItemNames items = map (sanitizeItemName items)
