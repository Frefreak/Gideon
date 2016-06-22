{-# LANGUAGE OverloadedStrings #-}
module XML.Server where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Network.Wreq
import Control.Lens.Operators
import Control.Monad.Trans.Class

import Auth
import Constant
import Types

serverStatusUrl :: String
serverStatusUrl = "https://api.eveonline.com/server/ServerStatus.xml.aspx"

getServerStatus :: Options -> String -> GideonMonad LBS.ByteString
getServerStatus opts uid = do
    r <- lift $ getWith opts serverStatusUrl
    return $ r ^. responseBody

