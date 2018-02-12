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
        data Entailment = I Line
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

subexpressions :: Expr -> SET
subexpressions e@(Enot se) = SET e [subexpressions se]
subexpressions e@(Eimp cond cons) = SET e [subexpressions cond, subexpressions cons]
subexpressions e@(Eite cond cons alt) = SET e [subexpressions cond, subexpressions cons, subexpressions alt]
subexpressions e@(Eand ses) = SET e $ map subexpressions ses
subexpressions e@(Eor ses)  = SET e $ map subexpressions ses
subexpressions e@(Exor ses) = SET e $ map subexpressions ses
subexpressions e@(Eiff ses) = SET e $ map subexpressions ses
subexpressions e@(Esym _) = SET e []
subexpressions Etrue  = SET Etrue []
subexpressions Efalse = SET Efalse []

{- Returns the list of in the given expression, as a list of expressions which
are guaranted to be of form (Esym String).
-}
symbols :: Expr -> [Expr]
symbols (Enot se) = symbols se
symbols (Eimp cond cons) = nub $ symbols cond ++ symbols cons
symbols (Eite cond cons alt) = nub $ symbols cond ++ symbols cons ++ symbols alt
symbols (Eand ses) = nub $ concatMap symbols ses
symbols (Eor ses)  = nub $ concatMap symbols ses
symbols (Exor ses) = nub $ concatMap symbols ses
symbols (Eiff ses) = nub $ concatMap symbols ses
symbols s@(Esym _) = [s]
symbols _  = []  -- Etrue, Efalse

{- toCNF, given an expression E, returns a list of ALWAYS EIGHT tuples whose
first element is (another list of tuples whose first element is the
subexpression before the predefined transformation and whose second element is
the self-same subexpression after the transformation), and whose second element
is resultant expression E' that is equivalent to E.
-}
toCNF :: Expr -> [([(Expr, Expr)], Expr)]
toCNF expr =
    let
    -- 0. Eliminate all if-then-else (ITE) subexpressions
        (eITE, pITE) = (nub $ eliminationsITE        expr,  canonical $ eliminateAllITE    expr)
    -- 1. Eliminate all if-and-only-if (IFF) subexpressions
        (eIFF, pIFF) = (nub $ eliminationsIFF        pITE,  canonical $ eliminateAllIFF    pITE)
    -- 2. Eliminate all implies (IMP) subexpressions
        (eIMP, pIMP) = (nub $ eliminationsIMP        pIFF,  canonical $ eliminateAllIMP    pIFF)
    -- 3. Eliminate all exclusive-org XOR subexpressions
        (eDNF, pDNF) = (nub $ eliminationsXORcnf     pIMP,  canonical $ eliminateAllXORcnf pIMP)
    -- 4. Distribute NOTs
        (dNT2, pNT2) = (nub $ distributionsNOT       pDNF,  canonical $ distributeAllNOT   pDNF)
    -- 5. Distribute ORs over ANDs
        (dOAN, pOAN) = (nub $ distributionsORAND     pNT2,  canonical $ distributeAllORAND    pNT2)
    in
        [(eITE, pITE), (eIFF, pIFF), (eIMP, pIMP), (eDNF, pDNF), (dNT2, pNT2), (dOAN, pOAN)]

prop_toCNF :: Expr -> Bool
prop_toCNF = isCNF . snd . last . toCNF

{- toDNF, given an expression E, returns a list of ALWAYS EIGHT tuples whose
first element is (another list of tuples whose first element is the
subexpression before the predefined transformation and whose second element is
the self-same subexpression after the transformation), and whose second element
is resultant expression E' that is equivalent to E.
-}
toDNF :: Expr -> [([(Expr, Expr)], Expr)]
toDNF expr =
    let
    -- 0. Eliminate all if-then-else (ITE) subexpressions
        (eITE, pITE) = (nub $ eliminationsITE    expr,  canonical $ eliminateAllITE    expr)
    -- 1. Eliminate all if-and-only-if (IFF) subexpressions
        (eIFF, pIFF) = (nub $ eliminationsIFF    pITE,  canonical $ eliminateAllIFF    pITE)
    -- 2. Eliminate all implies (IMP) subexpressions
        (eIMP, pIMP) = (nub $ eliminationsIMP    pIFF,  canonical $ eliminateAllIMP    pIFF)
    -- 3. Eliminate all exclusive-or (XOR) subexpressions
        (eDNF, pDNF) = (nub $ eliminationsXORcnf pIMP,  canonical $ eliminateAllXORcnf pIMP)
    -- 4. Distribute NOTs
        (dNOT, pNOT) = (nub $ distributionsNOT   pDNF,  canonical $ distributeAllNOT   pDNF)
    -- 5. Distribute ANDs over ORs
        (dAOR, pAOR) = (nub $ distributionsANDOR pNOT,  canonical $ distributeAllANDOR pNOT)
    in
        [(eITE, pITE), (eIFF, pIFF), (eIMP, pIMP), (eDNF, pDNF), (dNOT, pNOT), (dAOR, pAOR)]

prop_toDNF :: Expr -> Bool
prop_toDNF = isDNF . snd . last . toDNF

canonical :: Expr -> Expr
canonical = removeTriviality . removeRedundancy . flatten

{- Removes *syntactically* redundant (i.e. repeating) subexpressions in AND, OR,
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
removeRedundancy (Eand subexprs) = case nub $ map removeRedundancy subexprs of
    [x] -> x
    subexprs' -> eAND subexprs'
removeRedundancy (Eor subexprs) = case nub $ map removeRedundancy subexprs of
    [x] -> x
    subexprs' -> Eor subexprs'
removeRedundancy expr = expr

{- Removes trivial.
-}
removeTriviality :: Expr -> Expr
removeTriviality (Eand subexprs) =
    let subexprs' = filter (/= Etrue) $ map removeTriviality subexprs
    in  if   Efalse `elem` subexprs' || any (\se -> Enot se `elem` subexprs') subexprs'
       then Efalse
       else case subexprs' of
           [x] -> x
           _   -> eAND subexprs'
removeTriviality (Eor subexprs) =
    let subexprs' = filter (/= Efalse) $ map removeTriviality subexprs
    in  if   Etrue `elem` subexprs' || any (\se -> Enot se `elem` subexprs') subexprs'
      then Etrue
      else case subexprs' of
          [x] -> x
          _   -> Eor subexprs'
removeTriviality expr = expr

distributeANDOR :: Expr -> Maybe Expr
distributeANDOR (Eand subexprs) =
    let orSubexprs    = filter (\se -> case se of Eor _ -> True; _ -> False) subexprs
        nonOrSubexprs = filter (`notElem` orSubexprs) subexprs
    in  if   null orSubexprs
        then Nothing
        else Just $ Eor $ map (\x -> eAND $ nonOrSubexprs ++ x) (combine $ map (\(Eor x) -> x) orSubexprs)
distributeANDOR _ = Nothing

distributeAllANDOR :: Expr -> Expr
distributeAllANDOR = replaceD distributeANDOR

distributionsANDOR :: Expr -> [(Expr, Expr)]
distributionsANDOR = yieldD distributeANDOR

distributeORAND :: Expr -> Maybe Expr
distributeORAND (Eor subexprs) =
    let andSubexprs    = filter (\se -> case se of Eand _ -> True; _ -> False) subexprs
        nonAndSubexprs = filter (`notElem` andSubexprs) subexprs
    in  if   null andSubexprs
        then Nothing
        else Just $ eAND $ map (\x -> eOR $ nonAndSubexprs ++ x) (combine $ map (\(Eand x) -> x) andSubexprs)
distributeORAND _ = Nothing

distributeAllORAND :: Expr -> Expr
distributeAllORAND = replaceD distributeORAND

distributionsORAND :: Expr -> [(Expr, Expr)]
distributionsORAND = yieldD distributeORAND

-- Depth First Yield
yieldD :: (Expr -> Maybe Expr) -> Expr -> [(Expr, Expr)]
yieldD func expr =
    let innerYield = (case expr of
            Enot subexpr       -> yieldD func subexpr
            Eimp cond cons     -> yieldD func cond ++ yieldD func cons
            Eite cond cons alt -> yieldD func cond ++ yieldD func cons ++ yieldD func alt
            Eand subexprs      -> concatMap (yieldD func) subexprs
            Exor subexprs      -> concatMap (yieldD func) subexprs
            Eor  subexprs      -> concatMap (yieldD func) subexprs
            Eiff subexprs      -> concatMap (yieldD func) subexprs
            _ -> []
            )
        newX = (case expr of
            Enot subexpr       -> Enot (replaceD func subexpr)
            Eimp cond cons     -> Eimp (replaceD func cond) (replaceD func cons)
            Eite cond cons alt -> Eite (replaceD func cond) (replaceD func cons) (replaceD func alt)
            Eand subexprs      -> eAND $ map (replaceD func) subexprs
            Exor subexprs      -> eXOR $ map (replaceD func) subexprs
            Eor  subexprs      -> eOR  $ map (replaceD func) subexprs
            Eiff subexprs      -> eIFF $ map (replaceD func) subexprs
            _ -> expr
            )
    in  case func newX of
        Just expr' -> innerYield ++ [(canonical newX, canonical expr')] ++ yieldD func expr'
        Nothing    -> innerYield

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
flatten (Eite cond cons alt) = Eite (flatten cons) (flatten cons) (flatten alt)
flatten (Enot subexpr)  = Enot $ flatten subexpr
flatten expr | isSymbol expr = expr
             | otherwise     = error "programmer error! update flatten for new non-symbols!"

{- eval, given a list of true symbols, false symbols, and an expression, returns
a tuple where the first element of the tuple is a another tuple of list of
expressions for the symbols in trueSymbols list and falseSymbols lists
(respectively) that do NOT exist in the expression supplied, and the second
element of the returned tuple is another tuple whose first element is the
partially-evaluated expression after the CNF-based elimination, second element
is the final result of partial evaluation in DNF form.

eval supports partial evaluation.
-}
eval :: [Expr] -> [Expr] -> Expr -> EvalResult
eval trueSymbols falseSymbols expr =
    let cnf = snd $ last $ toCNF expr
        pos = clausalForm cnf  -- product of sums
        pTE = evalCNF trueSymbols falseSymbols pos
        dnf =  snd $ last $ toDNF pTE
        sop = clausalForm dnf  -- sum of products
    in  EvalResult { redundantTrueSymbols  = filter (`notElem` symbols expr) trueSymbols
                   , redundantFalseSymbols = filter (`notElem` symbols expr) falseSymbols
                   , cnf                   = cnf
                   , trueEliminations      = evaluationsCNF trueSymbols falseSymbols pos
                   , postTrueElimination   = pTE
                   , dnf                   = dnf
                   , falseEliminations     = evaluationsDNF trueSymbols falseSymbols sop
                   , postFalseElimination  = evalDNF trueSymbols falseSymbols sop
                   }

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

-- replace, deep first!
replaceD :: (Expr -> Maybe Expr) -> Expr -> Expr
replaceD func expr =
    let expr' = (case expr of
            Enot subexpr       -> Enot (replaceD func subexpr)
            Eimp cond cons     -> Eimp (replaceD func cond) (replaceD func cons)
            Eite cond cons alt -> Eite (replaceD func cond) (replaceD func cons) (replaceD func alt)
            Eand subexprs      -> eAND $ map (replaceD func) subexprs
            Exor subexprs      -> eXOR $ map (replaceD func) subexprs
            Eor  subexprs      -> eOR  $ map (replaceD func) subexprs
            Eiff subexprs      -> eIFF $ map (replaceD func) subexprs
            _ -> expr
            )
    in  case func expr' of
        Just newExpr -> replaceD func newExpr
        Nothing      -> expr'

-- TODO: RENAME "ELIMINATE*" as "TRANSFORM"
{- eliminateITE eliminates the given if-then-else expression of form (Γ ? Δ : Ω)
by replacing it with ((Γ ^ Δ) v (!Γ ^ Ω)); if the given expression is of another
form, then Nothing.
-}
eliminateITE :: Expr -> Maybe Expr
eliminateITE (Eite cond cons alt) = Just $ Eor [Eand [cond, cons], Eand [Enot cond, alt]]
eliminateITE _ = Nothing

eliminateAllITE :: Expr -> Expr
eliminateAllITE = replaceD eliminateITE

{- eliminateIFF eliminates the given if-and-only-if expression of form
(Γ1 <=> Γ2 <=> ... <=> Γn) by replacing it with (!(Γ1 + Γ2 + ... + Γn)); if the
given expression is of another form, then Nothing.
-}
eliminateIFF :: Expr -> Maybe Expr
eliminateIFF (Eiff ses) = Just $ Enot $ Exor ses
eliminateIFF _ = Nothing

eliminateAllIFF :: Expr -> Expr
eliminateAllIFF = replaceD eliminateIFF

{- eliminateIMP eliminates the given implies expressions of form (Γ1 => Γ2) by
replacing it with (!Γ1 v Γ2); if the given expression is of another form, then
Nothing.
-}
eliminateIMP :: Expr -> Maybe Expr
eliminateIMP (Eimp cond cons) = Just $ Eor [Enot cond, cons]
eliminateIMP _ = Nothing

eliminateAllIMP :: Expr -> Expr
eliminateAllIMP = replaceD eliminateIMP

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

eliminationsITE :: Expr -> [(Expr, Expr)]
eliminationsITE = yieldD eliminateITE

eliminationsIFF :: Expr -> [(Expr, Expr)]
eliminationsIFF = yieldD eliminateIFF

eliminationsIMP :: Expr -> [(Expr, Expr)]
eliminationsIMP = yieldD eliminateIMP

eliminationsXORdnf :: Expr -> [(Expr, Expr)]
eliminationsXORdnf = yieldD eliminateXORdnf

eliminationsXORcnf :: Expr -> [(Expr, Expr)]
eliminationsXORcnf = yieldD eliminateXORcnf

{-
EXAMPLE:
  > combine [['A', 'B', 'C'], ['1', '2']]
  ["A1","A2","B1","B2","C1","C2"]
-}
combine :: [[a]] -> [[a]]
combine (l:ls) = concatMap (\e -> map (\l2 -> e:l2) $ combine ls) l
combine [] = [[]]

combinations :: [a] -> Int -> [[a]]
combinations _  0 = [[]]  -- C(0, 0) = C(length s, 0) = 1,  ∀s
combinations [] _ = []  -- C(0, k) = 0,  k /= 0
combinations s 1 = map (: []) s
combinations s k
    | k >= 0 =
        concat $ map' (\h t -> (map (\l -> h : l) $ combinations t (k - 1)) ) s
    | otherwise = []  -- C(length s, k) = 0,  k < 0
    where
        map' :: (a -> [a] -> b) -> [a] -> [b]
        map' f [_] = []
        map' f (x:xs) = f x xs : map' f xs

prop_combinations :: [a] -> Int -> Bool
prop_combinations s k
    | length s < 20 && k < length s && k >= 0 = length (combinations s k) == c (length s) k
    | otherwise = True
    where
        c :: Int -> Int -> Int
        c s k =
            let s' = fromIntegral s
                k' = fromIntegral k
            in  fromIntegral $ fact s' `div` (fact k' * fact (s' - k'))

        fact :: Integer -> Integer
        fact n
            | n > 0     = product [1..n]
            | n == 0    = 1
            | otherwise = error "please don't make me calculate the factorial of a negative number"
