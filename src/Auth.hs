{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Auth where

import Data.List (intercalate)
import System.Process
import System.FilePath
import System.Environment
import Servant
import Network.Wai.Handler.Warp
import Control.Concurrent
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
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
import qualified Data.Yaml as Y

import Network.HTTP.Client (HttpException(..))

import Constant
import Database
import Util
import Types

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
        Right r' -> saveToken (r' ^. responseBody) >>= \n ->
                    case n of
                        Left err -> return (Left err)
                        Right n' -> do
                            saveCurrentUser n'
                            return (Right $ "Hello " ++ n')

saveCurrentUser :: String -> IO ()
saveCurrentUser n = do
    meta <- getMetaDataFile
    Y.encodeFile meta (GideonMetadata n)

saveToken :: LBS.ByteString -> IO (Either SomeException String)
saveToken r = do
    let accessToken = r ^. key "access_token" . _String
        refreshToken = r ^. key "refresh_token" . _String
    try (obtainCharacterInfo . T.unpack $ accessToken) >>= \char -> do
        if isLeft char then let Left err = char in return (Left err) else do
            let Right char' = char
            sql <- getSqlUser
            r <- try $ runSqlite sql $ do
                runMigration migrateAll
                insert $ Character (characterName char')
                    (show . coefficient $ characterID char')
                            (T.unpack accessToken) (T.unpack refreshToken)
            if isLeft r then let Left r' = r in return (Left r')
                else return (Right $ characterName char')

updateToken :: LBS.ByteString -> IO ()
updateToken r = do
    let accessToken = r ^. key "access_token" . _String
        refreshToken = r ^. key "refresh_token" . _String
    sql <- getSqlUser
    runSqlite sql $ do
        runMigration migrateAll
        updateWhere [CharacterRefreshToken ==. T.unpack refreshToken]
            [CharacterAccessToken =. T.unpack accessToken]

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
    h <- runCommand $ "xdg-open \"" ++ authUrl ++ "\""
    waitForProcess h
    readyFlag <- newEmptyMVar
    tid <- forkIO $ run callbackPort $
                serve callbackAPI (callbackServer sec readyFlag uniqState)
    takeMVar readyFlag
    threadDelay 1000000 -- delay 1s
    killThread tid

getAccessTokenRefresh :: String -> GideonMonad String
getAccessTokenRefresh refresh = do
    sec <- lift getSecretKey
    let opts = gideonOpt & auth ?~ basicAuth (BS.pack clientID) (BS.pack sec)
        toPost = ["grant_type" := ("refresh_token" :: String),
                    "refresh_token" := refresh]
    r <- lift . try $ postWith opts urlVerify toPost
    case r of
        Left e -> throwE (SE e)
        Right r' -> do
            rr <- lift . try $ updateToken (r' ^. responseBody)
            case rr of
                Left e -> throwE (SE e)
                Right _ -> do
                    return . T.unpack $ r' ^. responseBody
                        . key "access_token" . _String

getCharacterDb :: String -> GideonMonad Character
getCharacterDb username = do
    sql <- lift getSqlUser
    r <- lift . try $ runSqlite sql $ do
        runMigration migrateAll
        selectFirst [CharacterUsername ==. username] []
    case r of
        Left err -> throwE err
        Right Nothing -> throwE NoSuchCharacterException
        Right (Just (Entity _ char)) -> return char

getAccessTokenDb :: String -> GideonMonad String
getAccessTokenDb username = characterAccessToken <$> getCharacterDb username

getUserIDDb :: String -> GideonMonad String
getUserIDDb username = characterUserID <$> getCharacterDb username

getRefreshTokenDb :: String -> GideonMonad String
getRefreshTokenDb username = characterRefreshToken <$> getCharacterDb username

wrapAction :: (Options -> UserIDType -> AccessTokenType -> GideonMonad a) ->
                Options ->  UserIDType -> AccessTokenType -> GideonMonad a
wrapAction action = \op uid at -> do
    r <- lift . try $ runExceptT (action op uid at)
    case r of
        Left e@(StatusCodeException s _ _)
            | s ^. statusCode == 403 || s ^. statusCode == 401 ->
                throwE $ InvalidTokenException (show e)
            | otherwise -> throwE $ HE e
        Right (Right r') -> return r'
        Right (Left err) -> throwE err

execute :: (Options -> UserIDType -> AccessTokenType -> GideonMonad a) ->
            GideonMonad a
execute action = do
    f <- lift $ getMetaDataFile
    meta <- lift $ Y.decodeFileEither f
    case meta of
        Left err -> throwE (PE $ show err)
        Right r -> do
            let username = currentUser r
            uid <- getUserIDDb username
            accesstoken <- getAccessTokenDb username
            let opts = gideonOpt & auth ?~ oauth2Bearer (BS.pack accesstoken)
            r <- lift $ runExceptT (wrapAction action opts uid accesstoken)
            case r of
                Right r' -> return r'
                Left (InvalidTokenException str) -> do
                    lift $ putStrLn $ "debug: " ++ str
                    ac <- getRefreshTokenDb username >>= getAccessTokenRefresh
                    let opts = gideonOpt & auth ?~ oauth2Bearer (BS.pack ac)
                    action opts uid ac
                Left e -> throwE e

execute' :: (Options -> UserIDType -> AccessTokenType -> GideonMonad a) -> IO (Either GideonException a)
execute' = runExceptT . execute

composeXMLUrl :: String -> Params -> String
composeXMLUrl url params = composeUrl (xmlUrl ++ url) params

composeCRESTUrl :: String -> String
composeCRESTUrl = (crestUrl ++)
