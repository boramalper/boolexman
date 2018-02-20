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
module Engine.Commands where

import Data.List (nub, delete, sort, sortBy, (\\))
import Debug.Trace
import Test.QuickCheck

import DataTypes
import Engine.Transformers
import Engine.Other
import Utils (cartesianProduct, combinations)

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

symbols = symbols'

tabulate :: Expr -> ([Expr], [[Bool]])
tabulate expr =
    let syms     = symbols expr
        subexprs = sortBy sortCriteria $ flattenSET $ subexpressions expr
        evals    = sort $ map (\(ts, fs) -> map (evalS ts fs) subexprs) $ evaluations expr
    in (subexprs, evals)
    where
        {- Used to sort subexpressions by the following rules:
            1. Symbols come before any other non-symbols.
               1 (a). Symbols are ordered by their names in ascending order.
            2. Non-symbols are ordered by the number of subexpressions they
               conatain in ascending order.
        -}
        sortCriteria :: Expr -> Expr -> Ordering
        sortCriteria (Esym a) (Esym b) = compare a b
        sortCriteria (Esym _) b        = LT
        sortCriteria a        (Esym b) = GT
        sortCriteria a        b        = compare (length $ flattenSET $ subexpressions a) (length $ flattenSET $ subexpressions b)

prop_tabulate :: Expr -> Bool
prop_tabulate expr = let (headers, rows) = tabulate expr
                     in     headers /= []
                         && allSymbolsFirst headers
                         && all (\row -> length row == length headers) rows
    where
        allSymbolsFirst :: [Expr] -> Bool
        allSymbolsFirst [x]    = True
        allSymbolsFirst (x:xs) = if   isSymbol x
                                 then allSymbolsFirst xs
                                 else not $ any isSymbol xs

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
        (eITE, pITE) = (nub $ eliminationsITE        expr,  normalise $ eliminateAllITE    expr)
    -- 1. Eliminate all if-and-only-if (IFF) subexpressions
        (eIFF, pIFF) = (nub $ eliminationsIFF        pITE,  normalise $ eliminateAllIFF    pITE)
    -- 2. Eliminate all implies (IMP) subexpressions
        (eIMP, pIMP) = (nub $ eliminationsIMP        pIFF,  normalise $ eliminateAllIMP    pIFF)
    -- 3. Eliminate all exclusive-org XOR subexpressions
        (eDNF, pDNF) = (nub $ eliminationsXORcnf     pIMP,  normalise $ eliminateAllXORcnf pIMP)
    -- 4. Distribute NOTs
        (dNT2, pNT2) = (nub $ distributionsNOT       pDNF,  normalise $ distributeAllNOT   pDNF)
    -- 5. Distribute ORs over ANDs
        (dOAN, pOAN) = (nub $ distributionsOR     pNT2,  normalise $ distributeAllOR    pNT2)
    in
        [(eITE, pITE), (eIFF, pIFF), (eIMP, pIMP), (eDNF, pDNF), (dNT2, pNT2), (dOAN, pOAN)]

prop_toCNF :: Expr -> Bool
prop_toCNF expr = let result = snd $ last $ toCNF expr
                  in  if   expr == result
                      then discard
                      else isCNF result && (expr == result || equivalent expr result)

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
        (eITE, pITE) = (nub $ eliminationsITE    expr,  normalise $ eliminateAllITE    expr)
    -- 1. Eliminate all if-and-only-if (IFF) subexpressions
        (eIFF, pIFF) = (nub $ eliminationsIFF    pITE,  normalise $ eliminateAllIFF    pITE)
    -- 2. Eliminate all implies (IMP) subexpressions
        (eIMP, pIMP) = (nub $ eliminationsIMP    pIFF,  normalise $ eliminateAllIMP    pIFF)
    -- 3. Eliminate all exclusive-or (XOR) subexpressions
        (eDNF, pDNF) = (nub $ eliminationsXORcnf pIMP,  normalise $ eliminateAllXORcnf pIMP)
    -- 4. Distribute NOTs
        (dNOT, pNOT) = (nub $ distributionsNOT   pDNF,  normalise $ distributeAllNOT   pDNF)
    -- 5. Distribute ANDs over ORs
        (dAOR, pAOR) = (nub $ distributionsAND pNOT,  normalise $ distributeAllAND pNOT)
    in
        [(eITE, pITE), (eIFF, pIFF), (eIMP, pIMP), (eDNF, pDNF), (dNOT, pNOT), (dAOR, pAOR)]

prop_toDNF :: Expr -> Bool
prop_toDNF expr = let result = snd $ last $ toDNF expr
                  in  if   expr == result
                      then if isDNF result then discard else False
                      else isDNF result && equivalent expr result

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

prop_eval :: Expr -> Bool
prop_eval expr = all (\(ts, fs) -> postFalseElimination (eval ts fs expr) == toExpr (evalS ts fs expr)) $ evaluations expr
    where
        toExpr :: Bool -> Expr
        toExpr True  = Etrue
        toExpr False = Efalse

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

        takeOne :: Show a => (a -> Bool) -> [a] -> a
        takeOne f l = head [x | x <- l, f x]

        recurse :: [Expr] -> [Expr] -> Entailment
        recurse conds exprs
            | any (`elem` conds) exprs = I $ Line conds exprs
            | any isAND conds = let s@(Eand andSubexprs) = takeOne isAND conds
                                in  Land (Line conds exprs) $ recurse (nub $ delete s conds ++ andSubexprs) exprs
            | any isOR  exprs = let s@(Eor orSubexprs) = takeOne isOR exprs
                                in  Ror (Line conds exprs) $ recurse conds (nub $ delete s exprs ++ orSubexprs)
            | any isOR  conds = let s@(Eor orSubexprs) = takeOne isOR conds
                                    conds' = delete s conds
                                in  Lor (Line conds exprs) $ nub $ map (\ose -> recurse (ose:conds') exprs) orSubexprs
            | any isAND exprs = let s@(Eand andSubexprs) = takeOne isAND exprs
                                    exprs' = delete s exprs
                                in  Rand (Line conds exprs) $ nub $ map (\ase -> recurse conds (ase:exprs')) andSubexprs
            | any isIMP conds = let s@(Eimp cond cons) = takeOne isIMP conds
                                    conds' = delete s conds
                                in  Limp (Line conds exprs) (recurse conds' (cond:exprs)) (recurse (cons:conds') exprs)
            | any isIMP exprs = let s@(Eimp cond cons) = takeOne isIMP exprs
                                    exprs' = delete s exprs
                                in  Rimp (Line conds exprs) $ recurse (nub $ cond:conds) (nub $ cons:exprs')
            | any isNOT conds = let s@(Enot subexpr) = takeOne isNOT conds
                                in  Lnot (Line conds exprs) $ recurse (delete s conds) (nub $ subexpr:exprs)
            | any isNOT exprs = let s@(Enot subexpr) = takeOne isNOT exprs
                                in  Rnot (Line conds exprs) $ recurse (nub $ subexpr:conds) (delete s exprs)
            | otherwise       = F (Line conds exprs)

prop_entail :: Expr -> Expr -> Bool
-- for all evaluations that make cond true, expr must be true as well
prop_entail cond expr = if   doesEntail $ entailment $ entail cond expr
                        then all (\(ts, fs) -> evalS ts fs $ Eimp cond expr) $ evaluations $ Eimp cond expr
                        else discard
    where
      doesEntail :: Entailment -> Bool
      doesEntail (I _) = True
      doesEntail (F _) = False
      doesEntail (Land _ subent) = doesEntail subent
      doesEntail (Ror  _ subent) = doesEntail subent
      doesEntail (Rimp _ subent) = doesEntail subent
      doesEntail (Lnot _ subent) = doesEntail subent
      doesEntail (Rnot _ subent) = doesEntail subent
      doesEntail (Limp _ subent1 subent2) = doesEntail subent1 && doesEntail subent2
      doesEntail (Lxor _ subent1 subent2) = doesEntail subent1 && doesEntail subent2
      doesEntail (Rxor _ subent1 subent2) = doesEntail subent1 && doesEntail subent2
      doesEntail (Liff _ subent1 subent2) = doesEntail subent1 && doesEntail subent2
      doesEntail (Riff _ subent1 subent2) = doesEntail subent1 && doesEntail subent2
      doesEntail (Lor  _ subents) = all doesEntail subents
      doesEntail (Rand _ subents) = all doesEntail subents

--

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

prop_resolve :: Expr -> Bool
prop_resolve expr = satisfiable (resolve expr) == any (\(ts, fs) -> postFalseElimination (eval ts fs expr) == Etrue) (evaluations expr)
    where
        satisfiable :: Resolution -> Bool
        satisfiable res = not ([[], [Efalse]] `elemN` (initialStep res) || any ([[], [Efalse]] `elemN`) (map snd (resolutionSteps res)))

        elemN :: Eq a => [a] -> [a] -> Bool
        elemN needles haystack = any (`elem` haystack) needles
