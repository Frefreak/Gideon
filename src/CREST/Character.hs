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

getContacts :: Options -> String -> GideonMonad LBS.ByteString
getContacts opts uid = do
    r <- lift $ getWith opts $
        "https://crest-tq.eveonline.com/characters/" ++ uid ++ "/contacts/"
    return $ r ^. responseBody
