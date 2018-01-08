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

{- toDNF, given an expression E, returns a list of ALWAYS EIGHT tuples whose
first element is (another list of tuples whose first element is the
subexpression before the predefined transformation and whose second element is
the self-same subexpression after the transformation), and whose second element
is resultant expression E' that is equivalent to E.

EIGHT TRANSFORMATIONS:
  1. eliminating if-then-else (ITE)
  2. eliminating if-and-only-if (IFF)
  3. eliminating implies (IMP)

  !! Only combinations of ANDs, ORs, XORs, and NOTs can be observed from now on.

  4. push NOTs inside ANDs and ORs, stopping at XORs

  !! Only the following forms of expressions can be observed from now on:
     ( Esym String )
     ( Enot $ Esym String )
     ( Eand _ )
     ( Eor _ )
     ( Exor _ )
     ( Enot $ Exor _ )

  5. eliminate subexpressions of form (Enot $ Exor _) by eliminateXORcnf
     followed by distributing Enot
  6. eliminate subexpressions of form (Exor _) by eliminateXORdnf
  7. push NOTs inside ANDs and ORs
  8. distribute ANDs over ORs or vice versa
-}
toDNF :: Expr -> [([(Expr, Expr)], Expr)]
toDNF expr =
    let (eITE, pITE) = (eliminationsITE expr,  eliminateAllITE expr)
        (eIFF, pIFF) = (eliminationsIFF pITE,  eliminateAllIFF pITE)
        (eIMP, pIMP) = (eliminationsIMP pIFF,  eliminateAllIMP pIFF)
        (dNOT, pNOT) = (distributionsNOT pIMP, distributeAllNOT pIMP)
    in  [(eITE, pITE), (eIFF, pIFF), (eIMP, pIMP), (dNOT, pNOT)]

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
    where
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

eliminateXORcnf :: Expr -> Maybe Expr
eliminateXORcnf = undefined

{-
eliminateNOTXOR :: Expr -> Expr
eliminateNOTXOR (Enot xe@(Exor _)) = distributeNOT $ eliminateXORdnf xe
eliminateNOTXOR (Enot se) = Enot $ eliminateNOTXOR se
eliminateNOTXOR (Eimp cond cons) = Eimp (eliminateNOTXOR cond) (eliminateNOTXOR cons)
eliminateNOTXOR (Eite cond cons alt) = Eite (eliminateNOTXOR cond) (eliminateNOTXOR cons) (eliminateNOTXOR alt)
eliminateNOTXOR (Eand ses) = Eand $ map eliminateNOTXOR ses
eliminateNOTXOR (Eor ses)  = Eor  $ map eliminateNOTXOR ses
eliminateNOTXOR (Exor ses) = Exor $ map eliminateNOTXOR ses
eliminateNOTXOR (Eiff ses) = Eiff $ map eliminateNOTXOR ses
eliminateNOTXOR e@_ = e  -- Esym, Etrue, Efalse
-}

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
