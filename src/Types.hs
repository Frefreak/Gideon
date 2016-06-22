{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where
import Data.Typeable
import Control.Exception
import Control.Monad.Trans.Except
import Data.Aeson
import GHC.Generics
import Network.HTTP.Client

data GideonException    = NoSuchCharacterException
                        | InvalidTokenException String
                        | SE SomeException
                        | PE String
                        | HE HttpException
    deriving (Show, Typeable)
instance Exception GideonException

type GideonMonad = ExceptT GideonException IO

data GideonMetadata = GideonMetadata
    { currentUser :: String
    } deriving (Show, Generic)
instance ToJSON GideonMetadata
instance FromJSON GideonMetadata

type AccessTokenType = String
type UserIDType = String
