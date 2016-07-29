{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- everything related to ID
module IDIndex where

import Data.Aeson
import Data.Aeson.Lens

import Control.Lens
import Control.Monad
import System.FilePath
import System.Directory
import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light
import qualified Data.Text as T
import Data.Text.IO as TIO
import Data.Either
import qualified Data.Yaml as Y
import Data.Text.Encoding (encodeUtf8)
import Data.Function
import Data.Char (toLower)
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM

import Constant
import Types

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

allSolarSystemsTxt :: IO FilePath
allSolarSystemsTxt = (</> "allSolarSystems.txt") <$> sdeExtractionPath

completeSolarSystemName :: T.Text -> IO [T.Text]
completeSolarSystemName prefix = do
    exist <- allSolarSystemsTxt >>= doesFileExist
    if not exist then error "Run gen AllSystemsMap first!" else do
        solars <- map (head . T.words) . T.lines <$> (allSolarSystemsTxt >>= TIO.readFile)
        return $ filter (on T.isPrefixOf (T.map toLower) prefix) solars

