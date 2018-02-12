module Engine.Commands where

import Data.List (nub)

import DataTypes
import Engine.Transformers

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
