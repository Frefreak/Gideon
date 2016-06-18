{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where
import Data.Typeable
import Control.Exception
import Control.Monad.Trans.Except
import Data.Aeson
import GHC.Generics

data GideonException    = NoSuchCharacterException
                        | ActionFailedException String
                        | SE SomeException
    deriving (Show, Typeable)
instance Exception GideonException

type GideonMonad = ExceptT GideonException IO

data GideonMetadata = GideonMetadata
    { currentUser :: String
    } deriving (Show, Generic)
instance ToJSON GideonMetadata
instance FromJSON GideonMetadata
