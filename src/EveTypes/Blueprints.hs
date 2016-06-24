{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
module EveTypes.Blueprints where

import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import System.FilePath
import Data.Maybe
import qualified Data.Yaml as Y
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T

import Constant


type Blueprints = HM.HashMap T.Text Value

blueprintsYaml :: IO FilePath
blueprintsYaml = (</> "fsd/blueprints.yaml") <$> databasePath

getBlueprints :: FilePath -> IO Blueprints
getBlueprints fp = fromJust <$> Y.decodeFile fp

getBlueprint :: T.Text -> Blueprints -> Value
getBlueprint bid = (HM.! bid)
