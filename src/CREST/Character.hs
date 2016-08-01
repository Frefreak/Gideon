{-# LANGUAGE OverloadedStrings #-}
module CREST.Character where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Monad.Reader
import Network.Wreq
import Control.Lens.Operators
import Control.Monad.IO.Class (liftIO)

import Types
import Util

getContacts :: Gideon LBS.ByteString
getContacts = do
    opts <- asks authOpt
    uid <- asks userID
    r <- liftIO $ getWith opts $ composeCRESTUrl $
            "characters/" ++ uid ++ "/contacts/"
    return $ r ^. responseBody

getLocation :: Gideon LBS.ByteString
getLocation = do
    opts <- asks authOpt
    uid <- asks userID
    r <- liftIO $ getWith opts $ composeCRESTUrl $
            "characters/" ++ uid ++ "/location/"
    return $ r ^. responseBody

