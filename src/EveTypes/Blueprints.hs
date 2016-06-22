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
import qualified Data.Yaml as Y
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T

data BlueprintSkill = BlueprintSkill
    { _blueprintSkillLevel :: Int
    , _blueprintSkillTypeID :: Int
    } deriving (Show)
makeFields ''BlueprintSkill

data BlueprintProduct = BlueprintProduct
    { _blueprintProductProbability :: Maybe Double
    , _blueprintProductQuantity :: Int
    , _blueprintProductTypeID :: Int
    } deriving (Show)
makeFields ''BlueprintProduct

data BlueprintMaterial = BlueprintMaterial
    { _blueprintMaterialQuantity :: Int
    , _blueprintMaterialTypeID :: Int
    } deriving (Show)
makeFields ''BlueprintMaterial

data Time = Time
    { _timeTime :: Int
    } deriving (Show)
makeFields ''Time

data BlueprintInvention = BlueprintInvention
    { _blueprintInventionMaterials :: Maybe [BlueprintMaterial]
    , _blueprintProducts :: Maybe [BlueprintProduct]
    , _blueprintSkills :: Maybe [BlueprintSkill]
    , _blueprintInventionTime :: Int
    } deriving (Show)
makeFields ''BlueprintInvention

data BlueprintManufacturing = BlueprintManufacturing
    { _blueprintManufacturingMaterials :: Maybe [BlueprintMaterial]
    , _blueprintManufacturingProducts :: [BlueprintProduct]
    , _blueprintManufacturingSkills :: Maybe [BlueprintSkill]
    , _blueprintManufacturingTime :: Int
    } deriving (Show)
makeFields ''BlueprintManufacturing

data BlueprintActivities = BlueprintActivities
    { _blueprintActivitiesCopying :: Maybe Time
    , _blueprintActivitiesInvention :: Maybe BlueprintInvention
    , _blueprintActivitiesManufacturing :: Maybe BlueprintManufacturing
    , _blueprintActivitiesResearchMaterial :: Maybe Time
    , _blueprintActivitiesResearchTime :: Maybe Time
    } deriving (Show)
makeFields ''BlueprintActivities

data Blueprint = Blueprint
    { _blueprintActivities :: BlueprintActivities
    , _blueprintTypeID :: Int
    , _blueprintMaxProductionLimit :: Int
    } deriving (Show)
makeFields ''Blueprint

instance FromJSON BlueprintSkill where
    parseJSON (Object v) = BlueprintSkill <$>
                            v .: "level" <*>
                            v .: "typeID"

instance FromJSON BlueprintProduct where
    parseJSON (Object v) = BlueprintProduct <$>
                            v .:? "probability" <*>
                            v .: "quantity" <*>
                            v .: "typeID"

instance FromJSON BlueprintMaterial where
    parseJSON (Object v) = BlueprintMaterial <$>
                            v .: "quantity" <*>
                            v .: "typeID"

instance FromJSON Time where
    parseJSON (Object v) = Time <$> v .: "time"

instance FromJSON BlueprintManufacturing where
    parseJSON (Object v) = BlueprintManufacturing <$>
                            v .:? "materials" <*>
                            v .: "products" <*>
                            v .:? "skills" <*>
                            v .: "time"

instance FromJSON BlueprintInvention where
    parseJSON (Object v) = BlueprintInvention <$>
                            v .:? "materials" <*>
                            v .:? "products" <*>
                            v .:? "skills" <*>
                            v .: "time"

instance FromJSON BlueprintActivities where
    parseJSON (Object v) = BlueprintActivities <$>
                            v .:? "copying" <*>
                            v .:? "invention" <*>
                            v .:? "manufacturing" <*>
                            v .:? "research_material" <*>
                            v .:? "research_time"

instance FromJSON Blueprint where
    parseJSON (Object v) = Blueprint <$>
                            v .: "activities" <*>
                            v .: "blueprintTypeID" <*>
                            v .: "maxProductionLimit"

type Blueprints = HM.HashMap T.Text Blueprint
