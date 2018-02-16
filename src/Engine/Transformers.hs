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
module Engine.Transformers where

import Data.List (nub)
import Test.QuickCheck

import DataTypes
import Engine.Other (negateIn, evalS, evaluations, equivalent)
import Utils (combine, combinations)

prop_transformerMaybe :: (Expr -> Maybe Expr) -> Expr -> Property
prop_transformerMaybe func expr = case func expr of
    Just expr' -> classify (expr == expr') "trivial" $ expr == expr' || equivalent expr expr'
    Nothing    -> discard

prop_transformerAll :: (Expr -> Expr) -> Expr -> Property
prop_transformerAll func expr = let expr' = func expr
                                in  classify (expr == expr') "trivial" $ expr == expr' || equivalent expr expr'

prop_transformations :: (Expr -> [(Expr, Expr)]) -> Expr -> Bool
prop_transformations func expr = all (\(e, e') -> e == e' || equivalent e e' ) $ func expr

normalise :: Expr -> Expr
normalise = removeTriviality . removeRedundancy . flatten

prop_normalise :: Expr -> Property
prop_normalise = prop_transformerAll normalise

{-| Removes *syntactically* redundant (i.e. repeating) subexpressions in AND, OR,
XOR, IFF expressions, such as:

  (A and B and C) or (A and B and C) or (G and H)
  ^~~~~~~~~~~~~~^
   syntactically
     redundant

HOWEVER, it does not remove/eliminate semantically redundant subexpressions such
as:

  (A and B and C) or (A and B)
  ^~~~~~~~~~~~~~^
    semantically
     redundant

TODO: write a function to remove semantically redundant subexpressions (in DNF
and CNF forms).

ALSO:

   ((A ^ B ^ C) v (A ^ B ^ !C) v ...
 = ((A ^ B) v (A ^ B) v ...

TODO: identify all cases of semantic redundancy!
-}
removeRedundancy :: Expr -> Expr
removeRedundancy = replace removeRedundancyMaybe
    where
        removeRedundancyMaybe :: Expr -> Maybe Expr
        removeRedundancyMaybe (Eand subexprs) = let subexprs' = nub subexprs
                                                in  if   subexprs /= subexprs'
                                                    then Just $ eAND subexprs'
                                                    else Nothing
        removeRedundancyMaybe (Eor  subexprs) = let subexprs' = nub subexprs
                                                in  if   subexprs /= subexprs'
                                                    then Just $ eOR subexprs'
                                                    else Nothing
        removeRedundancyMaybe _               = Nothing

prop_removeRedundancy :: Expr -> Property
prop_removeRedundancy = prop_transformerAll removeRedundancy

{- Removes trivial.
-}
removeTriviality :: Expr -> Expr
removeTriviality = replace removeTrivialityMaybe
    where
        removeTrivialityMaybe :: Expr -> Maybe Expr
        removeTrivialityMaybe (Eand subexprs) =
            let subexprs' = filter (/= Etrue) subexprs
            in  if   Efalse `elem` subexprs' || any (\se -> Enot se `elem` subexprs') subexprs'
                then Just Efalse
                else if   subexprs /= subexprs'
                     then Just $ eAND subexprs'
                     else Nothing
        removeTrivialityMaybe (Eor subexprs) =
            let subexprs' = filter (/= Efalse) subexprs
            in  if   Etrue `elem` subexprs' || any (\se -> Enot se `elem` subexprs') subexprs'
                then Just Etrue
                else if   subexprs /= subexprs'
                     then Just $ eOR subexprs'
                     else Nothing
        removeTrivialityMaybe _ = Nothing

prop_removeTriviality :: Expr -> Property
prop_removeTriviality = prop_transformerAll removeTriviality

-- flatten nested expressions
-- TODO: can we reduce the redundancy here?
flatten :: Expr -> Expr
flatten (Eand subexprs) = eAND $ concatMap (
    \se -> case se of
        Eand subexprs' -> subexprs'
        _ -> [se]
    ) $ map flatten subexprs
flatten (Eor subexprs) = eOR $ concatMap (
    \se -> case se of
        Eor subexprs' -> subexprs'
        _ -> [se]
    ) $ map flatten subexprs
flatten (Exor subexprs) = eXOR $ concatMap (
    \se -> case se of
        Exor subexprs' -> subexprs'
        _ -> [se]
    ) $ map flatten subexprs
flatten (Eiff subexprs) = eIFF $ concatMap (
    \se -> case se of
        Eiff subexprs' -> subexprs'
        _ -> [se]
    ) $ map flatten subexprs
flatten (Eimp cond cons) = Eimp (flatten cond) (flatten cons)
flatten (Eite cond cons alt) = Eite (flatten cond) (flatten cons) (flatten alt)
flatten (Enot subexpr)  = Enot $ flatten subexpr
flatten expr | isSymbol expr = expr
             | otherwise     = error "programmer error! update flatten for new non-symbols!"

prop_flatten :: Expr -> Property
prop_flatten = prop_transformerAll flatten

distributeAND :: Expr -> Maybe Expr
distributeAND (Eand subexprs) =
    let orSubexprs    = filter (\se -> case se of Eor _ -> True; _ -> False) subexprs
        nonOrSubexprs = filter (`notElem` orSubexprs) subexprs
    in  if   null orSubexprs
        then Nothing
        else Just $ Eor $ map (\x -> eAND $ nonOrSubexprs ++ x) (combine $ map (\(Eor x) -> x) orSubexprs)
distributeAND _ = Nothing

prop_distributeAND :: Expr -> Property
prop_distributeAND = prop_transformerMaybe distributeAND

distributeNOT :: Expr -> Maybe Expr
distributeNOT expr = case expr of
    Enot (Enot se)  -> Just se
    Enot (Eand ses) -> Just $ eOR  $ map Enot ses
    Enot (Eor  ses) -> Just $ eAND $ map Enot ses
    Enot Etrue      -> Just Efalse
    Enot Efalse     -> Just Etrue
    _ -> Nothing

prop_distributeNOT :: Expr -> Property
prop_distributeNOT = prop_transformerMaybe distributeNOT

distributeOR :: Expr -> Maybe Expr
distributeOR (Eor subexprs) =
    let andSubexprs    = filter (\se -> case se of Eand _ -> True; _ -> False) subexprs
        nonAndSubexprs = filter (`notElem` andSubexprs) subexprs
    in  if   null andSubexprs
        then Nothing
        else Just $ eAND $ map (\x -> eOR $ nonAndSubexprs ++ x) (combine $ map (\(Eand x) -> x) andSubexprs)
distributeOR _ = Nothing

prop_distributeOR :: Expr -> Property
prop_distributeOR = prop_transformerMaybe distributeOR

{-| eliminateITE eliminates the given if-then-else expression of form (Γ ? Δ : Ω)
by replacing it with ((Γ ^ Δ) v (!Γ ^ Ω)); if the given expression is of another
form, then Nothing.
-}
eliminateITE :: Expr -> Maybe Expr
eliminateITE (Eite cond cons alt) = Just $ Eor [Eand [cond, cons], Eand [Enot cond, alt]]
eliminateITE _ = Nothing

prop_eliminateITE :: Expr -> Property
prop_eliminateITE = prop_transformerMaybe eliminateITE

{-| eliminateIFF eliminates the given if-and-only-if expression of form
(Γ1 <=> Γ2 <=> ... <=> Γn) by replacing it with (!(Γ1 + Γ2 + ... + Γn)); if the
given expression is of another form, then Nothing.
-}
eliminateIFF :: Expr -> Maybe Expr
eliminateIFF (Eiff ses) = if   length ses `mod` 2 == 0
                          then Just $ Enot $ eXOR ses
                          else Just $ eXOR ses
eliminateIFF _ = Nothing

prop_eliminateIFF :: Expr -> Property
prop_eliminateIFF = prop_transformerMaybe eliminateIFF

{-| eliminateIMP eliminates the given implies expressions of form (Γ1 => Γ2) by
replacing it with (!Γ1 v Γ2); if the given expression is of another form, then
Nothing.
-}
eliminateIMP :: Expr -> Maybe Expr
eliminateIMP (Eimp cond cons) = Just $ Eor [Enot cond, cons]
eliminateIMP _ = Nothing

prop_eliminateIMP :: Expr -> Property
prop_eliminateIMP = prop_transformerMaybe eliminateIMP

eliminateXORcnf :: Expr -> Maybe Expr
eliminateXORcnf (Exor subexprs) =
    let negationCounts = [0,2..length subexprs]
        negationCombinations = concatMap (combinations subexprs) negationCounts :: [[Expr]]
        negatedSubexprs = map (negateIn subexprs) negationCombinations :: [[Expr]]
    -- reverse makes the output easier to understand (try it!)
    in  Just $ eAND $ reverse $ map eOR negatedSubexprs
eliminateXORcnf _ = Nothing

prop_eliminateXORcnf :: Expr -> Property
prop_eliminateXORcnf = prop_transformerMaybe eliminateXORcnf

-- replace, deep first!
replace :: (Expr -> Maybe Expr) -> Expr -> Expr
replace func expr =
    let recurse = replace func
        expr'   = (case expr of
            Enot subexpr       -> Enot $ recurse subexpr
            Eimp cond cons     -> Eimp (recurse cond) (recurse cons)
            Eite cond cons alt -> Eite (recurse cond) (recurse cons) (recurse alt)
            Eand subexprs      -> eAND $ map recurse subexprs
            Exor subexprs      -> eXOR $ map recurse subexprs
            Eor  subexprs      -> eOR  $ map recurse subexprs
            Eiff subexprs      -> eIFF $ map recurse subexprs
            _ -> expr
            )
    in  case func expr' of
        Just newExpr -> replace func newExpr
        Nothing      -> expr'

distributeAllAND :: Expr -> Expr
distributeAllAND = replace distributeAND

prop_distributeAllAND :: Expr -> Property
prop_distributeAllAND = prop_transformerAll distributeAllAND

distributeAllNOT :: Expr -> Expr
distributeAllNOT = replace distributeNOT

prop_distributeAllNOT :: Expr -> Property
prop_distributeAllNOT = prop_transformerAll distributeAllNOT

distributeAllOR :: Expr -> Expr
distributeAllOR = replace distributeOR

prop_distributeAllOR :: Expr -> Property
prop_distributeAllOR = prop_transformerAll distributeAllOR

eliminateAllITE :: Expr -> Expr
eliminateAllITE = replace eliminateITE

prop_eliminateAllITE :: Expr -> Property
prop_eliminateAllITE = prop_transformerAll eliminateAllITE

eliminateAllIFF :: Expr -> Expr
eliminateAllIFF = replace eliminateIFF

prop_eliminateAllIFF :: Expr -> Property
prop_eliminateAllIFF = prop_transformerAll eliminateAllIFF

eliminateAllIMP :: Expr -> Expr
eliminateAllIMP = replace eliminateIMP

prop_eliminateAllIMP :: Expr -> Property
prop_eliminateAllIMP = prop_transformerAll eliminateAllIMP

eliminateAllXORcnf :: Expr -> Expr
eliminateAllXORcnf = replace eliminateXORcnf

prop_eliminateAllXORcnf :: Expr -> Property
prop_eliminateAllXORcnf = prop_transformerAll eliminateAllXORcnf

-- Depth First Yield
yield :: (Expr -> Maybe Expr) -> Expr -> [(Expr, Expr)]
yield func expr =
    let replace'   = replace func
        recurse    = yield func
        innerYield = (case expr of
            Enot subexpr       -> recurse subexpr
            Eimp cond cons     -> recurse cond ++ recurse cons
            Eite cond cons alt -> recurse cond ++ recurse cons ++ recurse alt
            Eand subexprs      -> concatMap recurse subexprs
            Exor subexprs      -> concatMap recurse subexprs
            Eor  subexprs      -> concatMap recurse subexprs
            Eiff subexprs      -> concatMap recurse subexprs
            _ -> []
            )
        newX = (case expr of
            Enot subexpr       -> Enot $ replace' subexpr
            Eimp cond cons     -> Eimp (replace' cond) (replace' cons)
            Eite cond cons alt -> Eite (replace' cond) (replace' cons) (replace' alt)
            Eand subexprs      -> eAND $ map replace' subexprs
            Exor subexprs      -> eXOR $ map replace' subexprs
            Eor  subexprs      -> eOR  $ map replace' subexprs
            Eiff subexprs      -> eIFF $ map replace' subexprs
            _ -> expr
            )
    in  case func newX of
        Just expr' -> innerYield ++ [(normalise newX, normalise expr')] ++ recurse expr'
        Nothing    -> innerYield

distributionsAND :: Expr -> [(Expr, Expr)]
distributionsAND = yield distributeAND

distributionsNOT :: Expr -> [(Expr, Expr)]
distributionsNOT = yield distributeNOT

distributionsOR :: Expr -> [(Expr, Expr)]
distributionsOR = yield distributeOR

eliminationsITE :: Expr -> [(Expr, Expr)]
eliminationsITE = yield eliminateITE

eliminationsIFF :: Expr -> [(Expr, Expr)]
eliminationsIFF = yield eliminateIFF

eliminationsIMP :: Expr -> [(Expr, Expr)]
eliminationsIMP = yield eliminateIMP

eliminationsXORcnf :: Expr -> [(Expr, Expr)]
eliminationsXORcnf = yield eliminateXORcnf
