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

obtainCharacterInfo' :: Options -> String -> GideonMonad CharacterInfo
obtainCharacterInfo' opts _ = do
    r <- lift $ getWith opts urlCharacterInfo
    return . fromJust . decode $ r ^. responseBody

main :: IO ()
main = hspec $ do
    describe "test Auth functionality" $ do
        it "re-acquire access token automatically" $ do
            -- need to had a current user in database
            r' <- runExceptT
                (execute obtainCharacterInfo')
            let Right r = r'
            r `shouldSatisfy` (\ci -> characterName ci == "Carson Xyris")
