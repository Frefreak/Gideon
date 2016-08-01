module Util where

import System.Random
import System.Directory
import System.FilePath
import System.Process
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.IO
import Control.Monad
{-import qualified Data.Vector as V-}
import Control.Concurrent.MSem
import Control.Concurrent.Async
import qualified Data.Traversable as T

import Constant
import Types

generateRandomState :: IO String
generateRandomState = take 16 . randomRs ('a', 'z') <$> newStdGen

createAppRootifNeeded :: IO ()
createAppRootifNeeded = getAppRoot >>= createDirectoryIfMissing False

filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap predi f = foldr (\i acc -> if predi i then f i : acc else acc) []

-- require `xclip`
getClipboard :: IO T.Text
getClipboard = T.pack <$> readProcess "xclip" ["-selection", "clipboard", "-o"] []

-- require `xclio`
setClipboard :: LBS.ByteString -> IO ()
setClipboard msg = do
    (stdIn, _, _, h) <-
        createProcess (shell "xclip -selection clipboard") {std_in = CreatePipe}
    case stdIn of
        Just i -> do
            LBS.hPutStr i msg
            hClose i
            void $ waitForProcess h
        Nothing -> putStrLn "fail to set clipboard content"

composeXMLUrl :: String -> Params -> String
composeXMLUrl url = composeUrl (xmlUrl ++ url)

composeCRESTUrl :: String -> String
composeCRESTUrl = (crestUrl ++)

composeUrl :: String -> Params -> String
composeUrl baseUrl params = baseUrl </> "?" ++ paramString where
    paramString = intercalate "&" (map (\(a, b) -> a ++ "=" ++ b) params)

-- scope is space delimited
composeParams :: String -> String -> [String] -> String -> Params
composeParams uri cid scope state = let scopes = unwords scope
    in [("response_type", "code"), ("redirect_uri", uri),
        ("client_id", cid), ("scope", scopes), ("state", state)]

showJson :: LBS.ByteString -> IO ()
showJson s = do
    setClipboard s
    j <- readProcess "jsonFormatter" [] []
    putStr j

mapPool :: T.Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool ma f xs = do
    sem <- new ma
    mapConcurrently (with sem . f) xs

forPool :: T.Traversable t => Int -> t a -> (a -> IO b) -> IO (t b)
forPool ma = flip (mapPool ma)
