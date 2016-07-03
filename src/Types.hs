{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where
import Data.Typeable
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import GHC.Generics
import Network.HTTP.Client
import Network.Wreq
import Data.Default
import Data.Scientific
import qualified Data.Text as T

data GideonException    = NoSuchCharacterException
                        | InvalidTokenException String
                        | SE SomeException
                        | PE String
                        | HE HttpException
                        | OE String
    deriving (Show, Typeable)
instance Exception GideonException

data GideonMetadata = GideonMetadata
    { currentUser :: String
    } deriving (Show, Generic)

instance ToJSON GideonMetadata
instance FromJSON GideonMetadata

type AccessTokenType = String
type UserIDType = String

type StationIDType = Scientific
type RegionIDType = Scientific

data AuthInfo = AuthInfo
    { authOpt :: Options
    , userID :: UserIDType
    , accessToken :: AccessTokenType
    , charName :: String
    } deriving (Show)

newtype Gideon a = Gideon
    { unGideon :: ReaderT AuthInfo (ExceptT GideonException IO) a
    } deriving (  Functor
                , Applicative
                , Monad
                , MonadIO
                , MonadReader AuthInfo
                , MonadError GideonException)

runGideon :: Gideon a -> AuthInfo -> IO (Either GideonException a)
runGideon g authinfo = runExceptT (runReaderT (unGideon g) authinfo)

data MarketOrder = MarketOrder
    { moStationID :: T.Text
    , moVolEntered :: Int
    , moVolRemaining :: Int
    , moTypeID :: T.Text
    , moPrice :: Double
    , moOrderID :: T.Text
    } deriving (Show, Eq)

