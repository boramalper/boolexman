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
module Engine where

import Data.List
import Data.Maybe
import Debug.Trace
import Test.QuickCheck

import DataTypes
import Parser


-------------------------------------------------

-- RANDOM EXAMPLES
--   entail (A v B v C ^ D) (A ^ B ^ C => (E => (D => (Z => E))))

entail :: Expr -> Expr -> EntailmentResult
entail cond expr = let condPostITEelimination  = eliminateAllITE cond
                       exprPostITEelimination = eliminateAllITE expr
                   in  EntailmentResult { condITEeliminations    = eliminationsITE cond
                                        , condPostITEelimination = condPostITEelimination
                                        , exprITEeliminations    = eliminationsITE expr
                                        , exprPostITEelimination = exprPostITEelimination
                                        , entailment             = recurse [condPostITEelimination] [exprPostITEelimination]
                       }
    where
        {-
DONE        data Entailment = I Line
DONE                        | F Line  -- failure!
DONE                        | Land Line Entailment
DONE                        | Ror  Line Entailment
DONE                        | Lor  Line [Entailment]
DONE                        | Rand Line [Entailment]
DONE                        | Limp Line Entailment Entailment
DONE                        | Rimp Line Entailment
DONE                        | Lnot Line Entailment
DONE                        | Rnot Line Entailment

TODO: I feel there might be an optimised way for these...
                        | Lxor Line Entailment Entailment
                        | Rxor Line Entailment Entailment
                        | Liff Line Entailment Entailment
                        | Riff Line Entailment Entailment
        -}

        recurse :: [Expr] -> [Expr] -> Entailment
        recurse conds exprs
            | any (`elem` conds) exprs = I $ Line conds exprs
            | any isAND conds = let s@(Eand andSubexprs) = getBy isAND conds
                                in  Land (Line conds exprs) $ recurse (delete s conds ++ andSubexprs) exprs
            | any isOR  exprs = let s@(Eor orSubexprs) = getBy isOR exprs
                                in  Ror (Line conds exprs) $ recurse conds (delete s exprs ++ orSubexprs)
            | any isOR  conds = let s@(Eor orSubexprs) = getBy isOR conds
                                    conds' = delete s conds
                                in  Lor (Line conds exprs) $ map (\ose -> recurse (ose:conds') exprs) orSubexprs
            | any isAND exprs = let s@(Eand andSubexprs) = getBy isAND exprs
                                    exprs' = delete s exprs
                                in  Rand (Line conds exprs) $ map (\ase -> recurse conds (ase:exprs')) andSubexprs
            | any isIMP conds = let s@(Eimp cond cons) = getBy isIMP conds
                                    conds' = delete s conds
                                in  Limp (Line conds exprs) (recurse conds' (cond:exprs)) (recurse (cons:conds') exprs)
            | any isIMP exprs = let s@(Eimp cond cons) = getBy isIMP exprs
                                    exprs' = delete s exprs
                                in  Rimp (Line conds exprs) $ recurse (cond:conds) (cons:exprs')
            | any isNOT conds = let s@(Enot subexpr) = getBy isNOT conds
                                in  Lnot (Line conds exprs) $ recurse (delete s conds) (subexpr:exprs)
            | any isNOT exprs = let s@(Enot subexpr) = getBy isNOT exprs
                                in  Rnot (Line conds exprs) $ recurse (subexpr:conds) (delete s exprs)
            | otherwise = F (Line conds exprs)

getBy :: (a -> Bool) -> [a] -> a
getBy func = head . filter func

-------------------------------------------------

{-

EXAMPLES:
  https://www.inf.ed.ac.uk/teaching/courses/inf1/cl/tutorials/2017/solutions4.pdf

  resolve ((A or B or not D) and (!A or D or E) and (!A or !C or E) and (B or C or E) and (!B or D or !E))
  resolve ((A v B v !D) ^ (!A v D v E) ^ (!A v !C v E) ^ (B v C v E) ^ (!B v D v !E))
  resolve ((A v B) ^ (A v !B v !C) ^ (!A v D) ^ (!B v C v D) ^ (!B v !D) ^ (!A v B v !D))

-}

resolve :: Expr -> Resolution
resolve expr = let initialStep = clausalForm $ snd $ last $ toCNF expr
                   (resolutionSteps, clauseStatuses) = recurse initialStep
               in  Resolution { initialStep     = initialStep
                              , resolutionSteps = resolutionSteps
                              , clauseStatuses  = clauseStatuses
                              }
    where
        recurse :: [Clause] -> (ResolutionSteps, ClauseStatuses)
        recurse clauses
            | just (findSuitableResolvent clauses) =
                let (Just resolvent)  = findSuitableResolvent clauses
                    usedClauses       = filter (\clause -> resolvent `elem` clause || Enot resolvent `elem` clause) clauses
                    newClauses        = calcNewClauses resolvent clauses
                    strikenClauses    = filter shouldStrike newClauses
                    dict              = map (\c -> (c, ResolvedBy resolvent)) usedClauses ++ map (\c -> (c, Striken)) strikenClauses
                    (nextRL, nextCD) = recurse $ (clauses \\ usedClauses) ++ (newClauses \\ strikenClauses)
                in  ((resolvent, newClauses) : nextRL, dict ++ nextCD)
           | otherwise = ([], [])

        just :: Maybe a -> Bool
        just (Just _) = True
        just Nothing  = False

        calcNewClauses :: Resolvent -> [Clause] -> [Clause]
        calcNewClauses resolvent clauses = let positiveClauses = filter (\c ->      resolvent `elem` c) clauses
                                               negativeClauses = filter (\c -> Enot resolvent `elem` c) clauses
                                           in  map (uncurry $ merge resolvent) $ cartesianProduct positiveClauses negativeClauses

        merge :: Resolvent -> Clause -> Clause -> Clause
        merge resolvent positive negative = nub $ (resolvent `delete` positive) ++ (Enot resolvent `delete` negative)

        findSuitableResolvent :: [Clause] -> Maybe Expr
        findSuitableResolvent clauses = let symbols = nub $ concat clauses
                                            res     = filter (\sym -> any (sym `elem`) clauses && any (Enot sym `elem`) clauses) symbols
                                        in  if   not $ null res
                                            then Just $ head res
                                            else Nothing

        shouldStrike :: Clause -> Bool
        shouldStrike exprs = any (\expr -> Enot expr `elem` exprs) exprs

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct as bs = concatMap (\a -> map (\b -> (a, b)) bs) as


-------------------------------------------------


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
        []  -> Etrue -- TODO: are we sure?
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
        []  -> Efalse -- TODO: are we sure?
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

findOne :: Eq a => [a] -> [a] -> Maybe a
findOne [] _ = Nothing
findOne (n:needles) haystack = if n `elem` haystack then Just n else findOne needles haystack

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

distributeNOT :: Expr -> Maybe Expr
distributeNOT expr = case expr of
    Enot (Enot se)  -> Just se
    Enot (Eand ses) -> Just $ Eor  $ map Enot ses
    Enot (Eor  ses) -> Just $ eAND $ map Enot ses
    Enot Etrue      -> Just Efalse
    Enot Efalse     -> Just Etrue
    _ -> Nothing

distributeAllNOT :: Expr -> Expr
distributeAllNOT = replaceD distributeNOT

{- eliminateXORdnf eliminates the given XOR expression of form
Γ1 + Γ2 + ... + Γn by replacing it with OR-of all AND'd n combinations of
Γ1, Γ2, ..., Γn where in each combination odd number of Γs are non-negated
(i.e. 1, 3, 5, ..., n).

For instance, for (Γ1 + Γ2 + Γ3 + Γ4 + Γ5):
  ( Γ1 +  Γ2 +  Γ3 +  Γ4 +  Γ5)
≡
  ( Γ1 ^  Γ2 ^  Γ3 ^  Γ4 ^  Γ5)
                              number of non-negated Γs in each AND expression: 5

v (!Γ1 ^ !Γ2 ^  Γ3 ^  Γ4 ^  Γ5) v (!Γ1 ^  Γ2 ^ !Γ3 ^  Γ4 ^  Γ5)
v (!Γ1 ^  Γ2 ^  Γ3 ^ !Γ4 ^  Γ5) v (!Γ1 ^  Γ2 ^  Γ3 ^  Γ4 ^ !Γ5)
v ( Γ1 ^ !Γ2 ^ !Γ3 ^  Γ4 ^  Γ5) v ( Γ1 ^ !Γ2 ^  Γ3 ^ !Γ4 ^  Γ5)
v ( Γ1 ^ !Γ2 ^  Γ3 ^  Γ4 ^ !Γ5) v ( Γ1 ^  Γ2 ^ !Γ3 ^ !Γ4 ^  Γ5)
v ( Γ1 ^  Γ2 ^ !Γ3 ^  Γ4 ^ !Γ5) v ( Γ1 ^  Γ2 ^  Γ3 ^ !Γ4 ^ !Γ5)
                              number of non-negated Γs in each AND expression: 3

v (!Γ1 ^ !Γ2 ^ !Γ3 ^ !Γ4 ^  Γ5) v (!Γ1 ^ !Γ2 ^ !Γ3 ^  Γ4 ^ !Γ5)
v (!Γ1 ^ !Γ2 ^  Γ3 ^ !Γ4 ^ !Γ5) v (!Γ1 ^  Γ2 ^ !Γ3 ^ !Γ4 ^ !Γ5)
v ( Γ1 ^ !Γ2 ^ !Γ3 ^ !Γ4 ^ !Γ5)
                              number of non-negated Γs in each AND expression: 1

Total number of AND expressions: C(5, 5) + C(5, 3) + C(5, 1) = 1 + 10 + 5 = 16
-}
eliminateXORdnf :: Expr -> Maybe Expr
eliminateXORdnf (Exor subexprs) =
    let lsubexprs = length subexprs
        negationCounts = if lsubexprs `mod` 2 == 0 then [1,3..lsubexprs] else [0,2..lsubexprs]
        negationCombinations = concatMap (combinations subexprs) negationCounts :: [[Expr]]
        negatedSubexprs = map (negateIn subexprs) negationCombinations :: [[Expr]]
    -- reverse makes the output easier to understand (try it!)
    in  Just $ Eor $ reverse $ map Eand negatedSubexprs
eliminateXORdnf _ = Nothing

eliminateAllXORdnf :: Expr -> Expr
eliminateAllXORdnf = replaceD eliminateXORdnf

eliminateXORcnf :: Expr -> Maybe Expr
eliminateXORcnf (Exor subexprs) =
    let lsubexprs = length subexprs
        negationCounts = if lsubexprs `mod` 2 == 0 then [0,2..lsubexprs] else [1,3..lsubexprs]
        negationCombinations = concatMap (combinations subexprs) negationCounts :: [[Expr]]
        negatedSubexprs = map (negateIn subexprs) negationCombinations :: [[Expr]]
    -- reverse makes the output easier to understand (try it!)
    in  Just $ eAND $ reverse $ map Eor negatedSubexprs
eliminateXORcnf _ = Nothing

eliminateAllXORcnf :: Expr -> Expr
eliminateAllXORcnf = replaceD eliminateXORcnf

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

distributionsNOT :: Expr -> [(Expr, Expr)]
distributionsNOT = yieldD distributeNOT

eliminationsXORdnf :: Expr -> [(Expr, Expr)]
eliminationsXORdnf = yieldD eliminateXORdnf

eliminationsXORcnf :: Expr -> [(Expr, Expr)]
eliminationsXORcnf = yieldD eliminateXORcnf
