{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Manufacture where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Reader
import Data.Scientific
import Data.Monoid
import Text.Printf
import Control.Exception

import Auth
import Types
import SDEDrill
import MarketUtil
import Terminal
import Util

type Recipes = [Recipe]

data RecipeRaw where
    -- | Itemname -> [SolarSystemNames] -> Volume
    RRBuy :: T.Text -> [T.Text] -> Int -> RecipeRaw
    -- | Itemname -> SolarSystemName -> Volume
    RRSell :: T.Text -> T.Text -> Int -> RecipeRaw
    -- | Itemname -> TaxRate -> Volume
    RRPIImport :: T.Text -> Scientific -> PI -> Int -> RecipeRaw
    -- | Itemname -> TaxRate -> Volume
    RRPIExport :: T.Text -> Scientific -> PI -> Int -> RecipeRaw
    -- | Target Name
    RROpt :: T.Text -> RecipeRaw -> RecipeRaw -> RecipeRaw
    -- | Target Name
    RRSum :: T.Text -> RecipeRaw -> RecipeRaw -> RecipeRaw
    RRNull :: RecipeRaw
    deriving Show

data Recipe where
    -- | Itemname -> [SolarSystemNames] -> Volume
    RBuy :: T.Text -> [T.Text] -> Int -> Recipe
    -- | Itemname -> SolarSystemName -> Volume
    RSell :: T.Text -> T.Text -> Int -> Recipe
    -- | Itemname -> TaxRate -> Volume
    RPIImport :: T.Text -> Scientific -> PI -> Int -> Recipe
    -- | Itemname -> TaxRate -> Volume
    RPIExport :: T.Text -> Scientific -> PI -> Int -> Recipe
    ROpt :: T.Text -> Recipe -> Recipe -> Recipe
    RSum :: T.Text -> Recipe -> Recipe -> Recipe
    RNull :: Recipe
    deriving Show

data PI =
    R0
  | P1
  | P2
  | P3
  | P4
  deriving Show

piBaseCost :: PI -> Double
piBaseCost R0 = 4
piBaseCost P1 = 400
piBaseCost P2 = 7200
piBaseCost P3 = 60000 -- eve uni's wiki says its 60000
piBaseCost P4 = 1200000

-- *1.5 if launched by CC, ignore for now
exportFee :: Scientific -> PI -> Int -> Double
exportFee taxRate p vol =
    fromIntegral vol * piBaseCost p * toRealFloat taxRate

importFee :: Scientific -> PI -> Int -> Double
importFee taxRate p vol =
    fromIntegral vol * piBaseCost p * toRealFloat taxRate * 0.5

validateRecipe :: RecipeRaw -> IO Recipe
validateRecipe (RRBuy x1 x2 x3) =
    RBuy <$> sanitizeItemNameIO x1 <*> sanitizeSystemNamesIO x2 <*> pure x3
validateRecipe (RRSell x1 x2 x3) =
    RSell <$> sanitizeItemNameIO x1 <*> sanitizeSystemNameIO x2 <*> pure x3
validateRecipe (RRPIImport x1 x2 x3 x4) =
    RPIImport <$> sanitizeItemNameIO x1 <*> pure x2 <*> pure x3 <*> pure x4
validateRecipe (RRPIExport x1 x2 x3 x4) =
    RPIExport <$> sanitizeItemNameIO x1 <*> pure x2 <*> pure x3 <*> pure x4
validateRecipe (RROpt x rr1 rr2) = ROpt
                                <$> pure x
                                <*> validateRecipe rr1
                                <*> validateRecipe rr2
validateRecipe (RRSum x rr1 rr2) = RSum
                                <$> pure x
                                <*> validateRecipe rr1
                                <*> validateRecipe rr2
validateRecipe RRNull = return RNull

naniteRecipe :: [RecipeRaw]
naniteRecipe =
    [ RRBuy "nanites" ["rens", "hek"] (round $ 480 * 3.6)
    , dataChips
    , gelbiopaste
    , RRSell "nanite rep" "rens" 4800
    ]

dataChipsFromMarket = RRBuy "data chips" ["rens", "hek"] 480
dataChipsFromPI = sumRecipeRaw "data chips from PI"
    [ RRBuy "supertensile pla" ["rens", "hek"] 1600
    , RRBuy "microfiber shi" ["rens", "hek"] 1600
    , RRPIImport "supertensile pla" 0.09 P2 1600
    , RRPIImport "microfiber shi" 0.09 P2 1600
    , RRPIExport "data chips" 0.09 P3 480
    ]
dataChips = RROpt "data chips" dataChipsFromMarket dataChipsFromPI

gelFromMarket = RRBuy "gel-" ["rens", "hek"] 480
gelFromPI = sumRecipeRaw "Gel-biopaste"
    [ RRBuy "Oxides" ["rens", "hek"] 1600
    , RRBuy "biocells" ["rens", "hek"] 1600
    , RRBuy "superconductors" ["rens", "hek"] 1600
    , RRPIImport "oxides" 0.09 P2 1600
    , RRPIImport "biocells" 0.09 P2 1600
    , RRPIImport "superconductors" 0.09 P2 1600
    , RRPIExport "gel-" 0.09 P3 480
    ]
gelbiopaste = RROpt "Gel-matrix biopaste" gelFromMarket gelFromPI

optRecipeRaw :: T.Text -> [RecipeRaw] -> RecipeRaw
optRecipeRaw tar = foldr (RROpt tar) RRNull

sumRecipeRaw :: T.Text -> [RecipeRaw] -> RecipeRaw
sumRecipeRaw tar = foldr (RRSum tar) RRNull

perform :: Recipe -> Gideon (Double, [String])
perform (RBuy x1 x2 x3) = do
    allorders <- lookupItemSellOrdersInSystemsSorted x1 x2
    return $ go allorders x3
      where 
            go [] _ = (0, ["No remaining orders available"])
            go (ord:ords) num
                | moVolRemaining ord >= num =
                    let price = moPrice ord
                        staName = moStationName ord
                    in (fromIntegral num * (-price),
                        [printf "buy %s x %d in %s at %f"
                            x1 num staName price])
                | moVolRemaining ord < num =
                    let price = moPrice ord
                        staName = moStationName ord
                        remain = moVolRemaining ord
                        thisTotal = fromIntegral remain * (-price)
                        thisText = printf "buy %s x %d in %s at %f"
                                    (T.unpack x1) remain staName price
                        (ordsTotal, ordsText) = go ords (num - remain)
                    in (ordsTotal + thisTotal, thisText:ordsText)
            go _ _ = (0, ["should not appear"])
perform (RSell x1 x2 x3) = do
    bestOrders <- lookupItemNBestSellOrderInSystem x1 x2 1
    if null bestOrders
        then return (0, [printf "%s: No sell orders in %s" x1 x2])
        else do
            let ord = head bestOrders
            return
                (fromIntegral x3 * moPrice ord,
                [printf "sell %s x %d in %s at %f"
                    x1 x3 (moStationName ord) (moPrice ord)])
perform (RPIImport x1 x2 x3 x4) = return
    (negate $ importFee x2 x3 x4,
    [printf "import %s x %d with taxRate %f" x1 x4 (toRealFloat x2 :: Double)])
perform (RPIExport x1 x2 x3 x4) = return
    (negate $ exportFee x2 x3 x4,
    [printf "export %s x %d with taxRate %f" x1 x4 (toRealFloat x2 :: Double)])
perform (ROpt _ r1 r2) = do
    authinfo <- ask
    let wrapper = executeWithAuth authinfo
    [(c1, s1), (c2, s2)] <-
        liftIO $ forPool 2 [r1, r2] $ \r -> wrapper (perform r)
    if c1 >= c2
        then return (c1, s1)
        else return (c2, s2)
perform (RSum _ r1 r2) = do
    authinfo <- ask
    let wrapper = executeWithAuth authinfo
    [(c1, s1), (c2, s2)] <-
        liftIO $ forPool 2 [r1, r2] $ \r -> wrapper (perform r)
    return (c1 + c2, s1 ++ s2)
perform RNull = return (0, [])


runRecipe :: [Recipe] -> Gideon [(Double, [String])]
runRecipe rs = do
    authinfo <- ask
    let wrapper = executeWithAuth authinfo
    liftIO $ forPool 5 rs $ \r -> wrapper $ perform r

interpretResult :: [T.Text] -> [(Double, [String])] -> IO ()
interpretResult items results = do
    mapM_ printResult (zip items results)
    let allcost = sum $ map fst . filter ((<0) . fst) $ results
        allprofit = sum $ map fst . filter ((>=0) . fst) $ results
    putStrLn $ "all cost:  " <> redString (show allcost)
    putStrLn $ "all profit: " <> greenString (show allprofit)
    putStrLn $ "profit: " <> costString (allprofit + allcost)
      where
        costString c = if c <= 0
                        then redString (show c)
                        else greenString (show c)
        printResult (item, (cost, process)) =
            unless (null process) $ do -- RNull
                TIO.putStrLn $ item <> ": " <> T.pack (costString cost)
                mapM_ (\p -> putStrLn $ "    " ++ p) process

recipeItemName :: Recipe -> T.Text
recipeItemName (RBuy x1 _ _) = x1
recipeItemName (RSell x1 _ _) = x1
recipeItemName (RPIImport x1 _ _ _) = x1
recipeItemName (RPIExport x1 _ _ _) = x1
recipeItemName (ROpt x1 _ _) = x1
recipeItemName (RSum x1 _ _) = x1
recipeItemName RNull = ""

summaryRecipe' :: [Recipe] -> IO ()
summaryRecipe' rs = do
    res <- execute $ runRecipe rs
    case res of
        Right result -> interpretResult (map recipeItemName rs) result
        Left err -> throw err

summaryRecipe :: [RecipeRaw] -> IO ()
summaryRecipe = mapM validateRecipe >=> summaryRecipe'

test2 =
    [ RRBuy "Tangled Power Conduit" ["rens", "hek"] 20
    , RRBuy "Tripped Power Circuit" ["rens", "hek"] 100
    , RRBuy "Burned Logic Circuit" ["rens", "hek"] 100
    , RRSell "Small Ancillary Current Router I" "rens" 10
    ]
