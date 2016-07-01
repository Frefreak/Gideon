module Util where

import System.Random
import System.Directory
import System.FilePath
import Control.Exception
{-import qualified Data.Vector as V-}
import Data.Yaml

import Constant
import Types

generateRandomState :: IO String
generateRandomState = take 16 . randomRs ('a', 'z') <$> newStdGen

createAppRootifNeeded :: IO ()
createAppRootifNeeded = getAppRoot >>= createDirectoryIfMissing False

