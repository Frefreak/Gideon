{-# LANGUAGE OverloadedStrings #-}
module CREST.Map where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Monad.Reader
import Network.Wreq
import Control.Lens.Operators
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Aeson.Lens
import Control.Lens

import Types
import Util

getRegions :: Gideon LBS.ByteString
getRegions = do
    opts <- asks authOpt
    r <- liftIO $ getWith opts $ composeCRESTUrl "regions/"
    return $ r ^. responseBody

getConstellations :: Gideon LBS.ByteString
getConstellations = do
    opts <- asks authOpt
    r <- liftIO $ getWith opts $ composeCRESTUrl "constellations/"
    return $ r ^. responseBody

getRegionList :: Gideon [(T.Text, RegionIDType)]
getRegionList = do
    lbs <- getRegions
    return $ lbs ^.. key "items" . _Array . traverse . to (\o -> (,)
        (o ^. key "name" . _String) (o ^?! key "id" . _Number))

getRegionByID :: RegionIDType -> Gideon LBS.ByteString
getRegionByID rid = do
    opts <- asks authOpt
    r <- liftIO $ getWith opts $ composeCRESTUrl $ "regions/" ++ showSci rid ++ "/"
    return $ r ^. responseBody

getConstellationsOfRegion :: RegionIDType -> Gideon [(ConstellationIDType, T.Text)]
getConstellationsOfRegion rid = do
    lbs <- getRegionByID rid
    return $ lbs ^.. key "constellations" . _Array . traverse . to (\o -> (,)
        (o ^?! key "id" . _Number) (o ^. key "href" . _String))

getConstellationByID :: ConstellationIDType -> Gideon LBS.ByteString
getConstellationByID cid = do
    opts <- asks authOpt
    r <- liftIO $ getWith opts $ composeCRESTUrl $
        "constellations/" ++ showSci cid ++ "/"
    return $ r ^. responseBody

getSolarSystemsOfConstellation :: ConstellationIDType -> Gideon [(SolarSystemIDType, T.Text)]
getSolarSystemsOfConstellation cid = do
    lbs <- getConstellationByID cid
    return $ lbs ^.. key "systems" . _Array . traverse . to (\o -> (,)
        (o ^?! key "id" . _Number) (o ^. key "href" . _String))

getSolarSystems :: Gideon LBS.ByteString
getSolarSystems = do
    opts <- asks authOpt
    r <- liftIO $ getWith opts $ composeCRESTUrl "solarsystems/"
    return $ r ^. responseBody

getSolarSystemList :: Gideon [(T.Text, SolarSystemIDType)]
getSolarSystemList = do
    lbs <- getSolarSystems
    return $ lbs ^.. key "items" . _Array . traverse . to (\o -> (,)
        (o ^. key "name" . _String) (o ^?! key "id" . _Number))
