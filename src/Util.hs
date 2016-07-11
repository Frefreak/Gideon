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

filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap pred f ls = foldr (\i acc -> if pred i then f i : acc else acc) [] ls
