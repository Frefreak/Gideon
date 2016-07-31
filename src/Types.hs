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
type ConstellationIDType = Scientific
type SolarSystemIDType = Scientific
type CorpIDType = Scientific

showSci :: Scientific -> String
showSci = show . coefficient

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

type Param = (String, String)
type Params = [Param]

data MarketOrder = MarketOrder
    { moStationID :: T.Text
    , moVolEntered :: Int
    , moVolRemaining :: Int
    , moTypeID :: T.Text
    , moPrice :: Double
    , moOrderID :: T.Text
    } deriving (Show, Eq)

-- I'm in Thera!
data TheraWormholeConnection = TWC
    { twcOutSignatureID :: SignatureType
    , twcInSignatureID :: SignatureType
    , twcWormholeType :: T.Text
    , twcDesSolarSystem :: T.Text
    , twcDesRegion :: T.Text
    } deriving (Show, Eq)

type STPrefix = T.Text
type STSuffix = T.Text
data SignatureType = ST STPrefix STSuffix

instance Eq SignatureType where
    ST p1 _ == ST p2 _ = p1 == p2
instance Show SignatureType where
    show (ST p s) = T.unpack p ++ "-" ++ T.unpack s

-- currently only contains signature id
data ProbeScannerResult = PSR
    { psrSignatureID :: SignatureType
    } deriving (Show)
