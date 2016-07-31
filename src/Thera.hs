{-# LANGUAGE OverloadedStrings #-}
-- everything related to Thera
module Thera where

import Data.Aeson
import Data.Aeson.Lens
import Network.Wreq
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Control.Exception
import Data.List ((\\))

import Types
import Util

eveScoutApiWormholes :: String
eveScoutApiWormholes = "https://www.eve-scout.com/api/wormholes"


-- get Thera wormholes from eve-scout
getWormholeLBS :: IO LBS.ByteString
getWormholeLBS = do
    get eveScoutApiWormholes >>= \r -> return $ r ^. responseBody

parseWormholeConnections :: LBS.ByteString -> [TheraWormholeConnection]
parseWormholeConnections lbs =
    lbs ^.. _Array . traverse . to (\o -> uncurry (TWC
        (ST (o ^. key "signatureId" . _String) "" )
        (ST (o ^. key "wormholeDestinationSignatureId" . _String) "")
        (let t1 = o ^. key "sourceWormholeType" . key "name" . _String
            in if t1 /= "K162" then t1 else
            o ^. key "destinationWormholeType" . key "name" . _String))
        (o ^. key "destinationSolarSystem" . to (\dss -> 
            (dss ^. key "name" . _String,
            dss ^. key "region" . key "name" . _String))))

getTheraWormholeConnections :: IO [TheraWormholeConnection]
getTheraWormholeConnections = parseWormholeConnections <$> getWormholeLBS

parseProbeScanner :: T.Text -> [ProbeScannerResult]
parseProbeScanner =
    let parseSig t = ST (T.takeWhile (/= '-') t) (T.drop 1 $ T.dropWhile (/= '-') t)
        head' l = if null l then "" else head l
    in  map (PSR . parseSig . head' . T.words) . T.lines

getProbeScannerResult :: IO [ProbeScannerResult]
getProbeScannerResult = do
    text <- getClipboard
    return . parseProbeScanner $ text

getUnscannedSigID :: IO [SignatureType]
getUnscannedSigID = do
    eveScout <- getTheraWormholeConnections
    self <- getProbeScannerResult
    return $ map psrSignatureID self \\ map twcOutSignatureID eveScout

reportUnscannedSigID :: IO ()
reportUnscannedSigID = getUnscannedSigID >>= mapM_ print
