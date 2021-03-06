{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Auth where

import System.Process
import System.Environment
import Servant
import Network.Wai.Handler.Warp
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except
import Control.Monad.Reader
import Network.Wreq hiding (Proxy)
import Control.Lens.Operators
import Data.Maybe
import Data.Either
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Lens
import GHC.Generics
import Database.Persist.Sqlite
import Data.Scientific
import qualified Data.Yaml as Y

import Network.HTTP.Client (HttpException(..))

import Constant
import Database
import Util
import Types
import Terminal

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
    let atk = r ^. key "access_token" . _String
        refreshToken = r ^. key "refresh_token" . _String
    try (obtainCharacterInfo . T.unpack $ atk) >>= \char ->
        if isLeft char then let Left err = char in return (Left err) else do
            let Right char' = char
            sql <- getSqlUser
            tr <- try $ runSqlite sql $ do
                runMigration migrateAll
                insert $ Character (characterName char')
                    (show . coefficient $ characterID char')
                            (T.unpack atk) (T.unpack refreshToken)
            if isLeft tr then let Left r' = tr in return (Left r')
                else return (Right $ characterName char')

updateToken :: LBS.ByteString -> IO ()
updateToken r = do
    let atk = r ^. key "access_token" . _String
        refreshToken = r ^. key "refresh_token" . _String
    sql <- getSqlUser
    runSqlite sql $ do
        runMigration migrateAll
        updateWhere [CharacterRefreshToken ==. T.unpack refreshToken]
            [CharacterAccessToken =. T.unpack atk]

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
    parseJSON invalid = typeMismatch "CharacterInfo" invalid

obtainCharacterInfo :: AccessTokenType -> IO CharacterInfo
obtainCharacterInfo atk = do
    let opts = gideonOpt & auth ?~ oauth2Bearer (BS.pack atk)
    r <- getWith opts urlCharacterInfo
    return . fromJust . decode $ r ^. responseBody

obtainCharacterName :: String -> IO String
obtainCharacterName at = characterName <$> obtainCharacterInfo at

getAccessTokenInitial  :: IO ()
getAccessTokenInitial = do
    createAppRootifNeeded
    sec <- getSecretKey
    uniqState <- generateRandomState
    let authUrl = composeUrl urlSSO paras
        paras = composeParams callbackUri clientID allScope uniqState
    h <- runCommand $ "xdg-open \"" ++ authUrl ++ "\""
    _ <- waitForProcess h
    readyFlag <- newEmptyMVar
    tid <- forkIO $ run callbackPort $
                serve callbackAPI (callbackServer sec readyFlag uniqState)
    takeMVar readyFlag
    threadDelay 1000000 -- delay 1s
    killThread tid

getAccessTokenRefresh :: String -> IO String
getAccessTokenRefresh refresh = do
    sec <- getSecretKey
    let opts = gideonOpt & auth ?~ basicAuth (BS.pack clientID) (BS.pack sec)
        toPost = ["grant_type" := ("refresh_token" :: String),
                    "refresh_token" := refresh]
    r <- try $ postWith opts urlVerify toPost
    case r of
        Left e -> throwIO (SE e)
        Right r' -> do
            rr <- try $ updateToken (r' ^. responseBody)
            case rr of
                Left e -> throwIO (SE e)
                Right _ ->
                    return . T.unpack $ r' ^. responseBody
                        . key "access_token" . _String

getCharacterDb :: String -> IO Character
getCharacterDb username = do
    sql <- getSqlUser
    r <- try $ runSqlite sql $ do
        runMigration migrateAll
        selectFirst [CharacterUsername ==. username] []
    case r of
        Left (err :: GideonException) -> throwIO err
        Right Nothing -> throwIO NoSuchCharacterException
        Right (Just (Entity _ char)) -> return char

getAccessTokenDb :: String -> IO String
getAccessTokenDb username = characterAccessToken <$> getCharacterDb username

getUserIDDb :: String -> IO String
getUserIDDb username = characterUserID <$> getCharacterDb username

getRefreshTokenDb :: String -> IO String
getRefreshTokenDb username = characterRefreshToken <$> getCharacterDb username

wrapAction :: Gideon a -> Gideon a
wrapAction action = do
    authinfo <- ask
    r <- liftIO . try $ runGideon action authinfo
    case r of
        Left e@(StatusCodeException s _ _)
            | s ^. statusCode == 403 || s ^. statusCode == 401
                || s ^. statusCode == 400 ->
                throwError $ InvalidTokenException (show e)
            | otherwise -> throwError $ HE e
        Left e' -> throwError (OE $ show e')
        Right (Right r') -> return r'
        Right (Left err) -> throwError err

getAuthInfo :: IO AuthInfo
getAuthInfo = do
    f <- getMetaDataFile
    meta <- Y.decodeFileEither f
    case meta of
        Left err -> throwIO $ PE (show err)
        Right r -> do
            let username = currentUser r
            uid <- getUserIDDb username
            accesstoken <- getAccessTokenDb username
            let opts = gideonOpt & auth ?~ oauth2Bearer (BS.pack accesstoken)
            return (AuthInfo opts uid accesstoken username)

execute' :: Gideon a -> Gideon a
execute' action = do
    au <- ask
    r <- liftIO $ runGideon (wrapAction action) au
    case r of
        Right r' -> return r'
        Left (InvalidTokenException str) -> do
            liftIO $ putStrLn $ "\ESC[1;31mdebug\ESC[0m:\n" ++ str
            _ <- liftIO $ getRefreshTokenDb (charName au) >>= getAccessTokenRefresh
            newAuth <- liftIO getAuthInfo
            rr <- liftIO $ runGideon action newAuth
            case rr of
                Right rr' -> return rr'
                Left e -> throwError e
        Left e -> throwError e

execute :: Gideon a -> IO (Either GideonException a)
execute action = do
    authinfo <- try getAuthInfo
    case authinfo of
        Right auth' -> runGideon (execute' action) auth'
        Left err -> return (Left err)

-- unsafe mode, used in batch mode (no database query)
executeWithAuth :: AuthInfo -> Gideon a -> IO a
executeWithAuth authinfo action = do
    r <- runGideon action authinfo
    case r of
        Left err -> throwIO err
        Right r' -> return r'

switchAccount :: String -> IO ()
switchAccount user = do
    sql <- getSqlUser
    r <- runSqlite sql $ do
        runMigration migrateAll
        selectFirst [CharacterUsername ==. user] []
    case r of
        Nothing -> putStrLn $ redString "error" ++ ": invalid username"
        Just (Entity _ _) -> saveCurrentUser user


