{- boolexman -- boolean expression manipulator
Copyright (c) 2017 Mert Bora ALPER <bora@boramalper.org>

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
-}
module Engine.TransformersProps where

import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.QuickCheck (Property, classify, discard, testProperty, (==>))

import DataTypes
import Engine.Other (equivalent)
import Engine.Transformers

props :: TestTree
props = testGroup "Transformers"
    [ testProperty "prop_normalise"          prop_normalise
    , testProperty "prop_removeRedundancy"   prop_removeRedundancy
    , testProperty "prop_removeTriviality"   prop_removeTriviality
    , testProperty "prop_flatten"            prop_flatten
    , testProperty "prop_distributeAND"      prop_distributeAND
    , testProperty "prop_distributeNOT"      prop_distributeNOT
    , testProperty "prop_distributeOR"       prop_distributeOR
    , testProperty "prop_eliminateITE"       prop_eliminateITE
    , testProperty "prop_eliminateIFF"       prop_eliminateIFF
    , testProperty "prop_eliminateIMP"       prop_eliminateIMP
    , testProperty "prop_eliminateXORcnf"    prop_eliminateXORcnf
    , testProperty "prop_distributeAllAND"   prop_distributeAllAND
    , testProperty "prop_distributeAllNOT"   prop_distributeAllNOT
    , testProperty "prop_distributeAllOR"    prop_distributeAllOR
    , testProperty "prop_eliminateAllITE"    prop_eliminateAllITE
    , testProperty "prop_eliminateAllIFF"    prop_eliminateAllIFF
    , testProperty "prop_eliminateAllIMP"    prop_eliminateAllIMP
    , testProperty "prop_eliminateAllXORcnf" prop_eliminateAllXORcnf
    , testProperty "prop_distributionsAND"   prop_distributionsAND
    , testProperty "prop_distributionsNOT"   prop_distributionsNOT
    , testProperty "prop_distributionsOR"    prop_distributionsOR
    , testProperty "prop_eliminationsITE"    prop_eliminationsITE
    , testProperty "prop_eliminationsIFF"    prop_eliminationsIFF
    , testProperty "prop_eliminationsIMP"    prop_eliminationsIMP
    , testProperty "prop_eliminationsXORcnf" prop_eliminationsXORcnf
    ]

prop_transformerMaybe :: (Expr -> Maybe Expr) -> Expr -> Property
prop_transformerMaybe func expr = case func expr of
    Just expr' -> classify (expr == expr') "trivial" $ expr == expr' || equivalent expr expr'
    Nothing    -> discard

prop_transformerAll :: (Expr -> Expr) -> Expr -> Property
prop_transformerAll func expr = let expr' = func expr
                                in  classify (expr == expr') "trivial" $ expr == expr' || equivalent expr expr'

prop_transformations :: (Expr -> [(Expr, Expr)]) -> Expr -> Bool
prop_transformations func expr = all (\(e, e') -> e == e' || equivalent e e' ) $ func expr

prop_normalise :: Expr -> Property
prop_normalise = prop_transformerAll normalise

prop_removeRedundancy :: Expr -> Property
prop_removeRedundancy = prop_transformerAll removeRedundancy

prop_removeTriviality :: Expr -> Property
prop_removeTriviality = prop_transformerAll removeTriviality

prop_flatten :: Expr -> Property
prop_flatten = prop_transformerAll flatten

prop_distributeAND :: Expr -> Property
prop_distributeAND = prop_transformerMaybe distributeAND

prop_distributeNOT :: Expr -> Property
prop_distributeNOT = prop_transformerMaybe distributeNOT

prop_distributeOR :: Expr -> Property
prop_distributeOR = prop_transformerMaybe distributeOR

prop_eliminateITE :: Expr -> Expr -> Expr -> Property
prop_eliminateITE cond cons alt = prop_transformerMaybe eliminateITE $ Eite cond cons alt

prop_eliminateIFF :: [Expr] -> Property
prop_eliminateIFF subexprs = length subexprs >= 2 ==> prop_transformerMaybe eliminateIFF $ eIFF subexprs

prop_eliminateIMP :: Expr -> Expr -> Property
prop_eliminateIMP cond cons = prop_transformerMaybe eliminateIMP $ Eimp cond cons

prop_eliminateXORcnf :: [Expr] -> Property
prop_eliminateXORcnf subexprs = length subexprs >= 2 ==> prop_transformerMaybe eliminateXORcnf $ eXOR subexprs

prop_distributeAllAND :: Expr -> Property
prop_distributeAllAND = prop_transformerAll distributeAllAND

prop_distributeAllNOT :: Expr -> Property
prop_distributeAllNOT = prop_transformerAll distributeAllNOT

prop_distributeAllOR :: Expr -> Property
prop_distributeAllOR = prop_transformerAll distributeAllOR

prop_eliminateAllITE :: Expr -> Property
prop_eliminateAllITE = prop_transformerAll eliminateAllITE

prop_eliminateAllIFF :: Expr -> Property
prop_eliminateAllIFF = prop_transformerAll eliminateAllIFF

prop_eliminateAllIMP :: Expr -> Property
prop_eliminateAllIMP = prop_transformerAll eliminateAllIMP

prop_eliminateAllXORcnf :: Expr -> Property
prop_eliminateAllXORcnf = prop_transformerAll eliminateAllXORcnf

prop_distributionsAND :: Expr -> Bool
prop_distributionsAND = prop_transformations distributionsAND

prop_distributionsNOT :: Expr -> Bool
prop_distributionsNOT = prop_transformations distributionsNOT

prop_distributionsOR :: Expr -> Bool
prop_distributionsOR = prop_transformations distributionsOR

prop_eliminationsITE :: Expr -> Bool
prop_eliminationsITE = prop_transformations eliminationsITE

prop_eliminationsIFF :: Expr -> Bool
prop_eliminationsIFF = prop_transformations eliminationsIFF

prop_eliminationsIMP :: Expr -> Bool
prop_eliminationsIMP = prop_transformations eliminationsIMP

prop_eliminationsXORcnf :: Expr -> Bool
prop_eliminationsXORcnf = prop_transformations eliminationsXORcnf
