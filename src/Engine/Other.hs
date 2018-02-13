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
module Engine.Other where

import Data.List
import Data.Maybe
import Debug.Trace
import Test.QuickCheck

import qualified Safe as Safe

import DataTypes
import Parser
import Utils (findOne)

isDNF :: Expr -> Bool
isDNF (Eand xs) = all (\x -> isSymbol x || isNegSymbol x) xs
isDNF (Eor  xs) = all (
    \x -> isSymbol x || isNegSymbol x || case x of
        Eand xs' -> all (\x' -> isSymbol x' || isNegSymbol x') xs'
        _        -> False
    ) xs
isDNF x = isSymbol x || isNegSymbol x

isCNF :: Expr -> Bool
isCNF (Eor  xs) = all (\x -> isSymbol x || isNegSymbol x) xs
isCNF (Eand xs) = all (
    \x -> isSymbol x || isNegSymbol x || case x of
        Eor xs' -> all (\x' -> isSymbol x' || isNegSymbol x') xs'
        _       -> False
    ) xs
isCNF x = isSymbol x || isNegSymbol x

{-
EXAMPLE (pseudo):
  clausalForm (A v B v !D) ^ (!A v D v E) ^ (!A v !C v E) ^ (B v C v E) ^ (!B v D v !E)
  [[A, B, !D], [!A, D, E], [!A, !C, E], [B, C, E], [!B, D, !E]]
-}
clausalForm :: Expr -> [[Expr]]
clausalForm expr
    | isCNF expr = case expr of
        Eand subexprs -> map (
            \se -> case se of
                Eor subexprs' -> subexprs'
                _ -> [se]
            ) subexprs
        Eor subexprs -> [subexprs]
        _ -> [[expr]]
    | isDNF expr = case expr of
        Eor subexprs -> map (
            \se -> case se of
                Eand subexprs' -> subexprs'
                _ -> [se]
            ) subexprs
        Eand subexprs -> [subexprs]
        _ -> [[expr]]
    | otherwise  = error "clausalForm exists only for expression in CNF or DNF!"

evalCNF :: [Expr] -> [Expr] -> [[Expr]] -> Expr
evalCNF trueSymbols falseSymbols maxterms =
    let eliminatedMaxterms = map snd $ evaluationsCNF trueSymbols falseSymbols maxterms
        result = maxterms \\ eliminatedMaxterms
    in case result of
        []  -> Etrue
        [x] -> orResult x
        _   -> eAND $ map orResult result
    where
        orResult :: [Expr] -> Expr
        orResult [x] = x
        orResult xs  = eOR xs

evaluationsCNF :: [Expr] -> [Expr] -> [[Expr]] -> [(Expr, [Expr])]
evaluationsCNF trueSymbols falseSymbols maxterms = mapMaybe (\mt ->
        if   Etrue `elem` mt
        then Just (Etrue, mt)
        else case findOne trueSymbols mt of
            Just trueSymbol -> Just (trueSymbol, mt)
            Nothing -> case findOne (map Enot falseSymbols) mt of
                Just negatedFalseSymbol -> Just (negatedFalseSymbol, mt)
                Nothing -> Nothing
    ) maxterms

evalDNF :: [Expr] -> [Expr] -> [[Expr]] -> Expr
evalDNF trueSymbols falseSymbols minterms =
    let eliminatedMinterms = map snd $ evaluationsDNF trueSymbols falseSymbols minterms
        result = minterms \\ eliminatedMinterms
    in case result of
        []  -> Efalse
        [x] -> andResult x
        _   -> eOR $ map andResult result
    where
        andResult :: [Expr] -> Expr
        andResult [x] = x
        andResult xs  = eAND xs

evaluationsDNF :: [Expr] -> [Expr] -> [[Expr]] -> [(Expr, [Expr])]
evaluationsDNF trueSymbols falseSymbols minterms = mapMaybe (\mt ->
        if   Efalse `elem` mt
        then Just (Efalse, mt)
        else case findOne falseSymbols mt of
            Just falseSymbol -> Just (falseSymbol, mt)
            Nothing -> case findOne (map Enot trueSymbols) mt of
                Just negatedTrueSymbol -> Just (negatedTrueSymbol, mt)
                Nothing -> Nothing
    ) minterms

eliminateTrue :: [Expr] -> [[Expr]] -> Expr
eliminateTrue trueSymbols maxterms =
    let trueEliminations   = eliminationsTrue trueSymbols maxterms
        eliminatedMaxterms = map snd trueEliminations
        result             = maxterms \\ eliminatedMaxterms
    in  if   length result > 1
        then eAND $ map eOR result
        else eOR  $ Safe.head "OT 127" result

eliminationsTrue :: [Expr] -> [[Expr]] -> [(Expr, [Expr])]
eliminationsTrue _ [] = []
eliminationsTrue trueSymbols (mt:maxterms) =
    if   Etrue `elem` mt
    then eliminationsTrue trueSymbols maxterms
    else case findOne trueSymbols mt of
        Just aTrueSymbol -> (aTrueSymbol, mt) : eliminationsTrue trueSymbols maxterms
        Nothing          -> eliminationsTrue trueSymbols maxterms


eliminateFalse :: [Expr] -> [[Expr]] -> Expr
eliminateFalse falseSymbols minterms =
    let falseEliminations  = eliminationsFalse falseSymbols minterms
        eliminatedMinterms = map snd falseEliminations
        result             = minterms \\ eliminatedMinterms
    in  if   length result > 1
        then eOR  $ map eAND result
        else eAND $ Safe.head "OT 146" result

eliminationsFalse :: [Expr] -> [[Expr]] -> [(Expr, [Expr])]
eliminationsFalse _ [] = []
eliminationsFalse falseSymbols (mt:minterms) =
    if   Efalse `elem` mt
    then eliminationsFalse falseSymbols minterms
    else case findOne falseSymbols mt of
        Just aFalseSymbol -> (aFalseSymbol, mt) : eliminationsFalse falseSymbols minterms
        Nothing           -> eliminationsFalse falseSymbols minterms

{- negateIn negates all expressions in the first list that also occurs
in the second (list).
-}
negateIn :: [Expr] -> [Expr] -> [Expr]
negateIn (x:xs) neg =
    if x `elem` neg then
        Enot x : negateIn xs neg
    else
        x : negateIn xs neg
negateIn [] _ = []
