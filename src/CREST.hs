module CREST (
    module CREST.Character,
    module CREST.Market,
    module CREST.Map,
    showCrestJson
    ) where

import CREST.Character
import CREST.Market
import CREST.Map

import qualified Data.ByteString.Lazy.Char8 as LBS

import Auth
import Util
import Types

showCrestJson :: (Gideon LBS.ByteString) -> IO ()
showCrestJson act = do
    Right res <- execute act
    showJson res
