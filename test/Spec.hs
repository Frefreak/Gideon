import Test.Hspec

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.ByteString.Char8 as BS
import Network.Wreq
import Control.Lens.Operators
import Data.Maybe
import Data.Aeson

import Auth
import Types
import Constant

selectNoSuchCharacterException :: Either GideonException Int -> Bool
selectNoSuchCharacterException (Left NoSuchCharacterException) = True
selectNoSuchCharacterException _ = False

obtainCharacterInfo' :: String -> GideonMonad CharacterInfo
obtainCharacterInfo' accessToken = do
    let opts = gideonOpt & auth ?~ oauth2Bearer (BS.pack accessToken)
    r <- lift $ getWith opts urlCharacterInfo
    return . fromJust . decode $ r ^. responseBody

main :: IO ()
main = hspec $ do
    describe "test Auth functionality" $ do
        it "handle non-existent character correctly" $ do
            r <- runExceptT (performAction "non-exist" $ wrapAction undefined)
            r `shouldSatisfy` selectNoSuchCharacterException
        it "re-acquire access token automatically" $ do
            -- need to had this user in database
            r' <- runExceptT
                (performAction "Carson Xyris" $ wrapAction obtainCharacterInfo')
            let Right r = r'
            r `shouldSatisfy` (\ci -> characterName ci == "Carson Xyris")
