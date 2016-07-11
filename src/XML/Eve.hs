{-# LANGUAGE OverloadedStrings #-}
module XML.Eve where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Network.Wreq
import Control.Monad.Reader
import Control.Lens.Operators
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Lens
import Text.XML
import Text.XML.Cursor
import Data.Default

import Auth
import Types
import Constant

typeNameUrl :: String
typeNameUrl = composeXMLUrl "eve/TypeName.xml.aspx" []

getTypeName :: [T.Text] -> Gideon [(T.Text, T.Text)]
getTypeName ids = do
    opts <- asks authOpt
    let opts' = opts & param "ids" .~ [(T.intercalate "," ids)]
    r <- liftIO $ getWith opts' typeNameUrl
    let cursor = fromDocument $ parseLBS_ def $ r ^. responseBody
    return $ zip (cursor $// attribute "typeID")
                    (cursor $// attribute "typeName")


