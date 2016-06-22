{-# LANGUAGE OverloadedStrings #-}
module CREST.Character where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Network.Wreq
import Control.Lens.Operators
import Control.Monad.Trans.Class

import Auth
import Constant
import Types

getContacts :: Options -> UserIDType -> AccessTokenType ->
                GideonMonad LBS.ByteString
getContacts opts uid _ = do
    r <- lift $ getWith opts $ composeCRESTUrl $
            "characters/" ++ uid ++ "/contacts/"
    return $ r ^. responseBody

getLocation :: Options -> UserIDType -> AccessTokenType ->
                GideonMonad LBS.ByteString
getLocation opts uid _ = do
    r <- lift $ getWith opts $ composeCRESTUrl $
            "characters/" ++ uid ++ "/location/"
    return $ r ^. responseBody
