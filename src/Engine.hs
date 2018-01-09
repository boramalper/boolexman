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
import Test.QuickCheck

import Expression

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
    -- 1. Eliminate all if-then-else (ITE) subexpressions
        (eITE, pITE) = (eliminationsITE        expr,  eliminateAllITE       expr)
    -- 2. Eliminate all if-and-only-if (IFF) subexpressions
        (eIFF, pIFF) = (eliminationsIFF        pITE,  eliminateAllIFF       pITE)
    -- 3. Eliminate all implies (IMP) subexpressions
        (eIMP, pIMP) = (eliminationsIMP        pIFF,  eliminateAllIMP       pIFF)
    -- 4. Distribute NOTs
        (dNOT, pNOT) = (distributionsNOT       pIMP,  distributeAllNOT      pIMP)
    -- 5. Eliminate all subexpressions of form (Enot (Exor _))
        (eCNF, pCNF) = (eliminationsNOTXORcnf  pNOT,  eliminateAllNOTXORcnf pNOT)
    -- 6. Eliminate all XOR subexpressions
        (eDNF, pDNF) = (eliminationsXORcnf     pCNF,  eliminateAllXORcnf    pCNF)
    -- 7. Distribute NOTs
        (dNT2, pNT2) = (distributionsNOT       pDNF,  distributeAllNOT      pDNF)
    -- 8. Distribute ANDs over ORs
        (dOAN, pOAN) = (distributionsORAND     pNT2,  distributeAllORAND    pNT2)
    in
        [(eITE, pITE), (eIFF, pIFF), (eIMP, pIMP), (dNOT, pNOT), (eCNF, pCNF),
        (eDNF, pDNF), (dNT2, pNT2), (dOAN, pOAN)]

{- toDNF, given an expression E, returns a list of ALWAYS EIGHT tuples whose
first element is (another list of tuples whose first element is the
subexpression before the predefined transformation and whose second element is
the self-same subexpression after the transformation), and whose second element
is resultant expression E' that is equivalent to E.
-}
toDNF :: Expr -> [([(Expr, Expr)], Expr)]
toDNF expr =
    let
    -- 1. Eliminate all if-then-else (ITE) subexpressions
        (eITE, pITE) = (eliminationsITE        expr,  eliminateAllITE       expr)
    -- 2. Eliminate all if-and-only-if (IFF) subexpressions
        (eIFF, pIFF) = (eliminationsIFF        pITE,  eliminateAllIFF       pITE)
    -- 3. Eliminate all implies (IMP) subexpressions
        (eIMP, pIMP) = (eliminationsIMP        pIFF,  eliminateAllIMP       pIFF)
    -- 4. Distribute NOTs
        (dNOT, pNOT) = (distributionsNOT       pIMP,  distributeAllNOT      pIMP)
    -- 5. Eliminate all subexpressions of form (Enot (Exor _))
        (eCNF, pCNF) = (eliminationsNOTXORdnf  pNOT,  eliminateAllNOTXORdnf pNOT)
    -- 6. Eliminate all XOR subexpressions
        (eDNF, pDNF) = (eliminationsXORdnf     pCNF,  eliminateAllXORdnf    pCNF)
    -- 7. Distribute NOTs
        (dNT2, pNT2) = (distributionsNOT       pDNF,  distributeAllNOT      pDNF)
    -- 8. Distribute ANDs over ORs
        (dAOR, pAOR) = (distributionsANDOR     pNT2,  distributeAllANDOR    pNT2)
    in
        [(eITE, pITE), (eIFF, pIFF), (eIMP, pIMP), (dNOT, pNOT), (eCNF, pCNF),
        (eDNF, pDNF), (dNT2, pNT2), (dAOR, pAOR)]

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
        pos = flattenCNF cnf
        pTE = eliminateTrue trueSymbols pos
        flattenCNF (Eand maxterms) = map (\(Eor  maxterm) -> maxterm) maxterms
        flattenDNF (Eor minterms)  = map (\(Eand minterm) -> minterm) minterms
    in  EvalResult { redundantTrueSymbols  = filter (`notElem` symbols expr) trueSymbols
                   , redundantFalseSymbols = filter (`notElem` symbols expr) falseSymbols
                   , cnf                   = cnf
                   , trueEliminations      = eliminationsTrue trueSymbols pos
                   , postTrueElimination   = pTE
                   , dnf                   = snd $ last $ toDNF pTE
                   , falseEliminations     = eliminationsFalse trueSymbols $ flattenDNF pTE
                   , postFalseElimination  = eliminateFalse falseSymbols $ flattenDNF pTE
                   }

eliminateTrue :: [Expr] -> [[Expr]] -> Expr
eliminateTrue trueSymbols maxterms =
    let trueEliminations   = eliminationsTrue trueSymbols maxterms
        eliminatedMaxterms = map snd trueEliminations
    in  Eand $ map Eor $ maxterms \\ eliminatedMaxterms

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
    in  Eor $ map Eand $ minterms \\ eliminatedMinterms

eliminationsFalse :: [Expr] -> [[Expr]] -> [(Expr, [Expr])]
eliminationsFalse falseSymbols (mt:minterms) =
    if   Efalse `elem` mt
    then eliminationsFalse falseSymbols minterms
    else case findOne falseSymbols mt of
        Just aFalseSymbol -> (aFalseSymbol, mt) : eliminationsFalse falseSymbols minterms
        Nothing           -> eliminationsFalse falseSymbols minterms

{- replace takes a function and an expression, and iterates over every single
subexpression of the expression (including the expression itself), calling the
supplied function each time. If the supplied function returns Just an
expression, the expression returned will "replace" its predecessor (i.e. the
argument of the function), else (i.e. if Nothing is returned) then replace will
continue iterating.

WARNING:
  It is the responsibility of the supplied function to recurse into
  subexpressions of the expression it replaces!

replace is a "template" for all kinds of eliminate* functions.
-}
replace :: (Expr -> Maybe Expr) -> Expr -> Expr
replace func expr@(Enot subexpr) = fromMaybe
    (Enot $ replace func subexpr)
    $ func expr
replace func expr@(Eimp cond cons) = fromMaybe
    (Eimp (replace func cond) $ replace func cons)
    $ func expr
replace func expr@(Eite cond cons alt) = fromMaybe
    (Eite (replace func cond) (replace func cons) $ replace func alt)
    $ func expr
replace func expr@(Eand subexprs) = fromMaybe
    (Eand $ map (replace func) subexprs)
    $ func expr
replace func expr@(Eor subexprs) = fromMaybe
    (Eor $ map (replace func) subexprs)
    $ func expr
replace func expr@(Exor subexprs) = fromMaybe
    (Exor $ map (replace func) subexprs)
    $ func expr
replace func expr@(Eiff subexprs) = fromMaybe
    (Eiff $ map (replace func) subexprs)
    $ func expr
replace func expr@(Esym _) = fromMaybe expr $ func expr
replace func expr@Etrue    = fromMaybe expr $ func expr
replace func expr@Efalse   = fromMaybe expr $ func expr

descend :: (Expr -> Maybe Expr) -> (Expr -> Expr) -> Expr -> Maybe Expr
descend eliminate eliminateAll expr = case eliminate expr of
    Just expr' -> Just $ eliminateAll expr'
    Nothing -> Nothing

{- eliminateITE eliminates the given if-then-else expression of form (Γ ? Δ : Ω)
by replacing it with ((Γ ^ Δ) v (!Γ ^ Ω)); if the given expression is of another
form, then Nothing.
-}
eliminateITE :: Expr -> Maybe Expr
eliminateITE (Eite cond cons alt) = Just $ Eor [Eand [cond, cons], Eand [Enot cond, alt]]
eliminateITE _ = Nothing

eliminateAllITE :: Expr -> Expr
eliminateAllITE = replace $ descend eliminateITE eliminateAllITE

{- eliminateIFF eliminates the given if-and-only-if expression of form
(Γ1 <=> Γ2 <=> ... <=> Γn) by replacing it with (!(Γ1 + Γ2 + ... + Γn)); if the
given expression is of another form, then Nothing.
-}
eliminateIFF :: Expr -> Maybe Expr
eliminateIFF (Eiff ses) = Just $ Enot $ Exor ses
eliminateIFF _ = Nothing

eliminateAllIFF :: Expr -> Expr
eliminateAllIFF = replace $ descend eliminateIFF eliminateAllIFF

{- eliminateIMP eliminates the given implies expressions of form (Γ1 => Γ2) by
replacing it with (!Γ1 v Γ2); if the given expression is of another form, then
Nothing.
-}
eliminateIMP :: Expr -> Maybe Expr
eliminateIMP (Eimp cond cons) = Just $ Eor [Enot cond, cons]
eliminateIMP _ = Nothing

eliminateAllIMP :: Expr -> Expr
eliminateAllIMP = replace $ descend eliminateIMP eliminateAllIMP

distributeNOT :: Expr -> Maybe Expr
distributeNOT expr = case expr of
    Enot (Enot se)  -> Just se
    Enot (Eand ses) -> Just $ Eor  $ map Enot ses
    Enot (Eor  ses) -> Just $ Eand $ map Enot ses
    Enot Etrue      -> Just Efalse
    Enot Efalse     -> Just Etrue
    _ -> Nothing

distributeAllNOT :: Expr -> Expr
distributeAllNOT = replace $ descend distributeNOT distributeAllNOT

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
eliminateAllXORdnf = replace $ descend eliminateXORdnf eliminateAllXORdnf

-- TODO: undefined CHECK IF WORKS AS INTENDED!
eliminateXORcnf :: Expr -> Maybe Expr
eliminateXORcnf (Exor subexprs) =
    let lsubexprs = length subexprs
        negationCounts = if lsubexprs `mod` 2 == 0 then [0,2..lsubexprs] else [1,3..lsubexprs]
        negationCombinations = concatMap (combinations subexprs) negationCounts :: [[Expr]]
        negatedSubexprs = map (negateIn subexprs) negationCombinations :: [[Expr]]
    -- reverse makes the output easier to understand (try it!)
    in  Just $ Eand $ reverse $ map Eor negatedSubexprs
eliminateXORcnf _ = Nothing

eliminateAllXORcnf :: Expr -> Expr
eliminateAllXORcnf = replace $ descend eliminateXORcnf eliminateAllXORcnf

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

eliminateNOTXORcnf :: Expr -> Maybe Expr
eliminateNOTXORcnf (Enot subexpr@(Exor _)) =
    let Just expr' = eliminateXORdnf subexpr
    in  Just $ distributeAllNOT $ Enot expr'
eliminateNOTXORcnf _ = Nothing

eliminateAllNOTXORcnf :: Expr -> Expr
eliminateAllNOTXORcnf = replace $ descend eliminateNOTXORcnf eliminateAllNOTXORcnf

eliminateNOTXORdnf :: Expr -> Maybe Expr
eliminateNOTXORdnf (Enot subexpr@(Exor _)) =
    let Just expr' = eliminateXORcnf subexpr
    in  Just $ distributeAllNOT $ Enot expr'
eliminateNOTXORdnf _ = Nothing

eliminateAllNOTXORdnf :: Expr -> Expr
eliminateAllNOTXORdnf = replace $ descend eliminateNOTXORdnf eliminateAllNOTXORdnf

distributeANDOR :: Expr -> Maybe Expr
distributeANDOR (Eand subexprs) =
    let orSubexprs = filter (\se -> case se of Eor _ -> True; _ -> False) subexprs
        nonOrSubexprs = filter (`notElem` orSubexprs) subexprs
    in
        if not $ null orSubexprs then
            Just $ Eor $ map (\x -> Eand $ nonOrSubexprs ++ x) (combine $ map (\(Eor x) -> x) orSubexprs)
        else
            Nothing
distributeANDOR _ = Nothing

distributeAllANDOR :: Expr -> Expr
distributeAllANDOR = replace $ descend distributeANDOR distributeAllANDOR

distributeORAND :: Expr -> Maybe Expr
distributeORAND (Eor subexprs) =
    let andSubexprs = filter (\se -> case se of Eand _ -> True; _ -> False) subexprs
        nonAndSubexprs = filter (`notElem` andSubexprs) subexprs
    in
        if not $ null andSubexprs then
            Just $ Eand $ map (\x -> Eor $ nonAndSubexprs ++ x) (combine $ map (\(Eand x) -> x) andSubexprs)
        else
            Nothing
distributeORAND _ = Nothing

distributeAllORAND :: Expr -> Expr
distributeAllORAND = replace $ descend distributeORAND distributeAllORAND

{- yield takes a function and an expression, and iterates over every single
subexpression of the expression (including the expression itself), calling the
supplied function each time. If the supplied function returns Just an
expression, the expression returned will be added to the list of yielded values,
else (i.e. if Nothing is returned) then nothing will be added to the list; in
both cases the iteration will continue for all subexpressions.

yield is a "template" for all kinds of eliminations* functions.
-}
yield :: (Expr -> Maybe a) -> Expr -> [a]
yield func expr@(Enot subexpr) =
    maybeToList (func expr) ++ yield func subexpr
yield func expr@(Eimp cond cons) =
    maybeToList (func expr) ++ yield func cond ++ yield func cons
yield func expr@(Eite cond cons alt) =
    maybeToList (func expr) ++ yield func cond ++ yield func cons ++ yield func alt
yield func expr@(Eand subexprs) =
    maybeToList (func expr) ++ concatMap (yield func) subexprs
yield func expr@(Eor subexprs) =
    maybeToList (func expr) ++ concatMap (yield func) subexprs
yield func expr@(Exor subexprs) =
    maybeToList (func expr) ++ concatMap (yield func) subexprs
yield func expr@(Eiff subexprs) =
    maybeToList (func expr) ++ concatMap (yield func) subexprs
yield func expr@(Esym _) = maybeToList $ func expr
yield func expr@Etrue    = maybeToList $ func expr
yield func expr@Efalse   = maybeToList $ func expr

maybePair :: (a -> Maybe b) -> a -> Maybe (a, b)
maybePair func a = case func a of
    Just b -> Just (a, b)
    Nothing -> Nothing

distributionsNOT :: Expr -> [(Expr, Expr)]
distributionsNOT = yield $ maybePair distributeNOT

eliminationsITE :: Expr -> [(Expr, Expr)]
eliminationsITE = yield $ maybePair eliminateITE

eliminationsIFF :: Expr -> [(Expr, Expr)]
eliminationsIFF = yield $ maybePair eliminateIFF

eliminationsIMP :: Expr -> [(Expr, Expr)]
eliminationsIMP = yield $ maybePair eliminateIMP

eliminationsXORdnf :: Expr -> [(Expr, Expr)]
eliminationsXORdnf = yield $ maybePair eliminateXORdnf

eliminationsXORcnf :: Expr -> [(Expr, Expr)]
eliminationsXORcnf = yield $ maybePair eliminateXORcnf

eliminationsNOTXORcnf :: Expr -> [(Expr, Expr)]
eliminationsNOTXORcnf = yield $ maybePair eliminateNOTXORcnf

eliminationsNOTXORdnf :: Expr -> [(Expr, Expr)]
eliminationsNOTXORdnf = yield $ maybePair eliminateNOTXORdnf

distributionsANDOR :: Expr -> [(Expr, Expr)]
distributionsANDOR = yield $ maybePair distributeANDOR

distributionsORAND :: Expr -> [(Expr, Expr)]
distributionsORAND = yield $ maybePair distributeORAND

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
