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

import DataTypes
import Parser
import Utils (findOne, combinations)

-- strict eval!
-- intended for testing `eval`, `toDNF`, and `toCNF` commands
evalS :: [Expr] -> [Expr] -> Expr -> Bool
evalS trueSymbols falseSymbols expr
    |    all (\e -> isSymbol e && e `notElem` [Etrue, Efalse]) trueSymbols
      && all (\e -> isSymbol e && e `notElem` [Etrue, Efalse]) falseSymbols
      && all (\s -> (s `elem` trueSymbols) /= (s `elem` falseSymbols)) (symbols' expr)
      =
        recurse expr
    | otherwise = error $ "evalS failed!  (ts: " ++ show trueSymbols ++ ", fs: " ++ show falseSymbols ++ ", expr: " ++ show expr ++" )"
    where
        recurse :: Expr -> Bool
        recurse     (Enot subexpr)       = not $ recurse subexpr
        recurse     (Eimp cond cons)     = not (recurse cond) || recurse cons
        recurse     (Eite cond cons alt) = (recurse cond && recurse cons) || (not (recurse cond) && recurse alt)
        recurse     (Eand subexprs)      = all recurse subexprs
        recurse     (Eor  subexprs)      = any recurse subexprs
        recurse     (Exor subexprs)      = foldr1 xor $ map recurse subexprs --length [s | s <- subexprs, recurse s] `mod` 2 == 1
            where
                xor True  False = True
                xor False True  = True
                xor _     _     = False
        recurse     (Eiff subexprs)      = foldr1 iff $ map recurse subexprs -- length [s | s <- subexprs, not $ recurse s] `mod` 2 == 0
            where
                iff True  True  = True
                iff False False = True
                iff _     _     = False
        recurse sym@(Esym _)             = sym `elem` trueSymbols
        recurse      Etrue               = True
        recurse      Efalse              = False

-- | for a given expression, returns a list of all possible true/false symbols
evaluations :: Expr -> [([Expr], [Expr])]
evaluations expr = let syms = symbols' expr
                   in  concatMap (\n -> let trueSymbols = combinations syms n
                                        in  map (\ts -> (ts, syms \\ ts)) trueSymbols
                           ) [0..length syms]

tautology :: Expr -> Bool
tautology expr = expr == Etrue || equivalent Etrue expr

equivalent :: Expr -> Expr -> Bool
equivalent p q
    | p /= q = let syms = nub $ symbols' p ++ symbols' q
               in  all (\(ts, fs) -> evalS ts fs p == evalS ts fs q) $ evaluationsL syms
    | otherwise = error "p is equal to q and that's *probably* not what you wanted!"
    where
        evaluationsL :: [Expr] -> [([Expr], [Expr])]
        evaluationsL syms = concatMap (\n -> let trueSymbols = combinations syms n
                                             in  map (\ts -> (ts, syms \\ ts)) trueSymbols
                                ) [0..length syms]

{- Returns the list of in the given expression, as a list of expressions which
are guaranted to be of form (Esym String).
-}
symbols' :: Expr -> [Expr]
symbols' (Enot se) = symbols' se
symbols' (Eimp cond cons) = nub $ symbols' cond ++ symbols' cons
symbols' (Eite cond cons alt) = nub $ symbols' cond ++ symbols' cons ++ symbols' alt
symbols' (Eand ses) = nub $ concatMap symbols' ses
symbols' (Eor ses)  = nub $ concatMap symbols' ses
symbols' (Exor ses) = nub $ concatMap symbols' ses
symbols' (Eiff ses) = nub $ concatMap symbols' ses
symbols' s@(Esym _) = [s]
symbols' _  = []  -- Etrue, Efalse

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
        else eOR  $ head result

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
        else eAND $ head result

eliminationsFalse :: [Expr] -> [[Expr]] -> [(Expr, [Expr])]
eliminationsFalse _ [] = []
eliminationsFalse falseSymbols (mt:minterms) =
    if   Efalse `elem` mt
    then eliminationsFalse falseSymbols minterms
    else case findOne falseSymbols mt of
        Just aFalseSymbol -> (aFalseSymbol, mt) : eliminationsFalse falseSymbols minterms
        Nothing           -> eliminationsFalse falseSymbols minterms
