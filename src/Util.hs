module Util where

import System.Random
import System.Directory
import Control.Exception

import Constant

generateRandomState :: IO String
generateRandomState = take 16 . randomRs ('a', 'z') <$> newStdGen

createAppRootifNeeded :: IO ()
createAppRootifNeeded = getAppRoot >>= createDirectoryIfMissing False
