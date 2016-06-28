import Test.Hspec

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
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

obtainCharacterInfo' :: Options -> UserIDType -> AccessTokenType
    -> Gideon CharacterInfo
obtainCharacterInfo' opts _ _ = do
    r <- liftIO $ getWith opts urlCharacterInfo
    return . fromJust . decode $ r ^. responseBody

main :: IO ()
main = hspec $ do
    describe "test Auth functionality" $ do
        it "re-acquire access token automatically" $ do
            -- need to had a current user in database
            r' <- runGideon
                (execute obtainCharacterInfo')
            let Right r = r'
            r `shouldSatisfy` (\ci -> characterName ci == "Carson Xyris")
