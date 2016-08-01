{-# LANGUAGE OverloadedStrings #-}
module XML.Server where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.Wreq
import Control.Monad.Reader
import Control.Lens.Operators
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

import Types
import Util

serverStatusUrl :: String
serverStatusUrl = composeXMLUrl "server/ServerStatus.xml.aspx" []

getServerStatus :: Gideon LBS.ByteString
getServerStatus = do
    uid <- asks userID
    at <- asks accessToken
    opts <- asks authOpt
    let opts' = opts & param "characterID" .~ [T.pack uid]
                    & param "accessToken" .~ [T.pack at]
    r <- liftIO $ getWith opts' serverStatusUrl
    return $ r ^. responseBody

