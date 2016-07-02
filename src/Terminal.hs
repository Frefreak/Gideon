module Terminal where

import System.Console.ANSI

colorString :: Color -> String -> String
colorString col str = prefix ++ str ++ appendix where
    prefix = setSGRCode [SetColor Foreground Vivid col,
                        SetConsoleIntensity BoldIntensity]
    appendix = setSGRCode [Reset]

blackString :: String -> String
blackString = colorString Black

redString :: String -> String
redString = colorString Red

greenString :: String -> String
greenString = colorString Green

yellowString :: String -> String
yellowString = colorString Yellow

blueString :: String -> String
blueString = colorString Blue

magentaString :: String -> String
magentaString = colorString Magenta

cyanString :: String -> String
cyanString = colorString Cyan

whiteString :: String -> String
whiteString = colorString White

blinkString :: String -> String
blinkString str = prefix ++ str ++ appendix where
    prefix = setSGRCode [SetBlinkSpeed SlowBlink]
    appendix = setSGRCode [Reset]

blinkColorString :: Color -> String -> String
blinkColorString col = blinkString . colorString col
