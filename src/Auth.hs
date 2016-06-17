{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Auth where

import Data.List (intercalate)
import System.Process
import System.FilePath
import System.Environment
import Servant
import Network.Wai.Handler.Warp
import Control.Concurrent
import Control.Monad.Trans.Class (lift)
import Network.Wreq hiding (Proxy)
import Control.Lens.Operators
import Data.Maybe
import Data.Either
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Lens
import GHC.Generics
import Database.Persist.Sqlite
import Database.Persist
import Data.Scientific

import Constant
import Database
import Util

type Param = (String, String)
type Params = [Param]

composeUrl :: String -> Params -> String
composeUrl baseUrl params = baseUrl </> "?" ++ paramString where
    paramString = intercalate "&" (map (\(a, b) -> a ++ "=" ++ b) params)

composeParams :: String -> String -> [String] -> String -> Params
composeParams uri cid scope state = let scopes = intercalate " " scope
    in [("response_type", "code"), ("redirect_uri", uri),
        ("client_id", cid), ("scope", scopes), ("state", state)]

type CallbackAPI = "callback" :> QueryParam "code" String
            :> QueryParam "state" String :> Get '[PlainText] String

callbackServer :: String -> MVar () -> String -> Server CallbackAPI
callbackServer sec readyFlag es code state = do
    let shutdown = lift $ putMVar readyFlag ()
    if isNothing code || isNothing state then shutdown >> return "Failure"
        else if es == fromJust state then do
                result <- lift $ verifyAuthCode (fromJust code) sec
                case result of
                    Right r -> shutdown >> return r
                    Left e -> shutdown >> return (show e) else
                    shutdown >> return "Failure"

callbackAPI :: Proxy CallbackAPI
callbackAPI = Proxy

getSecretKey :: IO String
getSecretKey = getEnv "GideonSecretKey"

verifyAuthCode :: String -> String -> IO (Either SomeException String)
verifyAuthCode code sec = do
    let opts = gideonOpt & auth ?~ basicAuth (BS.pack clientID) (BS.pack sec)
        toPost = ["grant_type" := ("authorization_code" :: String),
                    "code" := code]
    r <- try $ postWith opts urlVerify toPost
    case r of
        Left err -> return (Left err)
        Right r' -> saveToken (r' ^. responseBody) >>=
                        \n -> return (("Hello " ++) <$>  n)

saveToken :: LBS.ByteString -> IO (Either SomeException String)
saveToken r = do
    let accessToken = r ^. key "access_token" . _String
        refreshToken = r ^. key "refresh_token" . _String
    try (obtainCharacterName . T.unpack $ accessToken) >>= \n -> do
        if isLeft n then return n else do
            let Right n' = n
            sql <- getSqlUser
            r <- try $ runSqlite sql $ do
                runMigration migrateAll
                insert $ Character n' (T.unpack accessToken)
                            (T.unpack refreshToken)
            if isLeft r then let Left r' = r in return (Left r')
                else return n

data CharacterInfo = CharacterInfo
    { characterID :: Scientific
    , characterName :: String
    , expiresOn :: String
    , scopes :: String
    , tokenType :: String
    , characterOwnerHash :: String
    } deriving (Show, Generic)

instance FromJSON CharacterInfo where
    parseJSON (Object v) = CharacterInfo <$>
                            v .: "CharacterID" <*>
                            v .: "CharacterName" <*>
                            v .: "ExpiresOn" <*>
                            v .: "Scopes" <*>
                            v .: "TokenType" <*>
                            v .: "CharacterOwnerHash"

obtainCharacterInfo :: String -> IO CharacterInfo
obtainCharacterInfo accessToken = do
    let opts = gideonOpt & auth ?~ oauth2Bearer (BS.pack accessToken)
    r <- getWith opts urlCharacterInfo
    return . fromJust . decode $ r ^. responseBody

obtainCharacterName :: String -> IO String
obtainCharacterName at = characterName <$> obtainCharacterInfo at

getAccessTokenInitial  :: IO ()
getAccessTokenInitial = do
    createAppRootifNeeded
    sec <- getSecretKey
    uniqState <- generateRandomState
    let authUrl = composeUrl urlSSO params
        params = composeParams callbackUri clientID allScope uniqState
    runCommand $ "xdg-open \"" ++ authUrl ++ "\""
    readyFlag <- newEmptyMVar
    tid <- forkIO $ run callbackPort $
                serve callbackAPI (callbackServer sec readyFlag uniqState)
    takeMVar readyFlag
    threadDelay 1000000 -- delay 1s
    killThread tid
