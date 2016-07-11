{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Network.Wreq
import Control.Lens.Operators
import Data.Maybe
import Data.Aeson

import Auth
import Types
import Constant
import CREST

selectNoSuchCharacterException :: Either GideonException Int -> Bool
selectNoSuchCharacterException (Left NoSuchCharacterException) = True
selectNoSuchCharacterException _ = False

obtainCharacterInfo' :: Gideon CharacterInfo
obtainCharacterInfo' = do
    opts <- asks authOpt
    r <- liftIO $ getWith opts urlCharacterInfo
    return . fromJust . decode $ r ^. responseBody

main :: IO ()
main = hspec $ do
    describe "test Auth functionality" $ do
        it "re-acquire access token automatically" $ do
            -- need to had a current user in database
            r' <- execute obtainCharacterInfo'
            let Right r = r'
            r `shouldSatisfy` (not . null . characterName)
    describe "test CREST.Market api" $ do
        it "getMarketType" $ do
            Right opts <- execute $ asks authOpt
            str <- getMarketType opts "11182"
            str `shouldSatisfy` (not . null)
        it "getMarketBuyOrders" $ do
            Right opts <- execute $ asks authOpt
            mos <- getMarketBuyOrders opts 10000002 "30834"
            mos `shouldSatisfy` \mo -> not (null mo) && moTypeID (head mo) == "30834"
        it "getMarketSellOrders" $ do
            Right opts <- execute $ asks authOpt
            mos <- getMarketSellOrders opts 10000002 "30832"
            mos `shouldSatisfy` \mo -> not (null mo) && moTypeID (head mo) == "30832"
