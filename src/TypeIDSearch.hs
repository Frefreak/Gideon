{-# LANGUAGE OverloadedStrings #-}
module TypeIDSearch where

import Data.Aeson
import Data.Aeson.Lens

import Control.Lens
import Control.Monad
import System.FilePath
import Text.Regex.PCRE.Heavy
import qualified Data.Text as T
import Data.Either
import qualified Data.Yaml as Y
import Data.Text.Encoding (encodeUtf8)

import qualified Data.HashMap.Lazy as HM

import Constant

type Item = Value
type Items = HM.HashMap T.Text Value

itemValid :: Item -> [Regex] -> Bool
itemValid it regs = and $ map containWord regs where
    desc = it ^. key "description" . key "en" . _String
    name = it ^. key "name" . key "en" . _String
    containWord wd = (desc =~ wd) || (name =~ wd)

filterItems :: Items -> [Regex] -> Items
filterItems items regs = HM.filter (flip itemValid regs) items

searchItems' :: Items -> T.Text -> HM.HashMap T.Text Value
searchItems' items inputs =
    let regs = rights $ map (flip compileM [] . encodeUtf8 . T.append "(?i)") $
                T.words inputs
    in filterItems items regs

searchItems :: Items -> T.Text -> HM.HashMap T.Text T.Text
searchItems items inputs = HM.map
    (\o -> o ^. key "name" . key "en" . _String) $ searchItems' items inputs

getItems :: IO (Maybe Items)
getItems = typeIDsYaml >>= Y.decodeFile

typeIDsYaml :: IO FilePath
typeIDsYaml = (</> "fsd/typeIDs.yaml") <$> databasePath
