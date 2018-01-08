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
    let (eITE, pITE) = (eliminationsITE expr,  eliminateITE expr)
        (eIFF, pIFF) = (eliminationsIFF pITE,  eliminateIFF pITE)
        (eIMP, pIMP) = (eliminationsIMP pIFF,  eliminateIMP pIFF)
        (dNOT, pNOT) = (distributionsNOT pIMP, distributeNOT pIMP)
    in  [(eITE, pITE), (eIFF, pIFF), (eIMP, pIMP), (dNOT, pNOT)]

{- replace takes a function and an expression, and iterates over every single
subexpression of the expression (including the expression itself), calling the
supplied function each time. If the supplied function returns Just an
expression, the expression returned will "replace" its predecessor (i.e. the
argument of the function), else (i.e. if Nothing is returned) then replace will
continue iterating.

replace is a "template" for all kinds of eliminate* functions.
-}
replace :: (Expr -> Maybe Expr) -> Expr -> Expr
{-
Enot Expr
| Eimp Expr Expr
| Eite Expr Expr Expr
| Eand [Expr]
| Eor  [Expr]
| Exor [Expr]
| Eiff [Expr]
| Esym String
| Etrue
| Efalse
-}
replace func expr@(Enot subexpr) = fromMaybe
    (Enot $ replace func subexpr)
    $ func expr
replace func expr@(Eimp cond cons) = fromMaybe
    (Eimp (replace func cond) $ replace func cons)
    $ func expr
replace func expr@(Eite cond cons alt) = fromMaybe
    (Eite (replace func cond) (replace func cons) $ replace func alt)
    $ func expr
replace func expr@(Eand subExprs) = fromMaybe
    (Eand $ map (replace func) subExprs)
    $ func expr
replace func expr@(Eor subExprs) = fromMaybe
    (Eor $ map (replace func) subExprs)
    $ func expr
replace func expr@(Exor subExprs) = fromMaybe
    (Exor $ map (replace func) subExprs)
    $ func expr
replace func expr@(Eiff subExprs) = fromMaybe
    (Eiff $ map (replace func) subExprs)
    $ func expr
replace func expr@(Esym _) = fromMaybe expr $ func expr
replace func expr@Etrue = fromMaybe expr $ func expr
replace func expr@Efalse = fromMaybe expr $ func expr


eliminateITE2 :: Expr -> Expr
eliminateITE2 = replace eITE2
    where
        eITE2 :: Expr -> Maybe Expr
        eITE2 (Eite cond cons alt) = Just $ Eor [Eand [eliminateITE2 cond, eliminateITE2 cons], Eand [Enot $ eliminateITE2 cond, eliminateITE2 alt]]
        eITE2 _ = Nothing

prop_ite :: Expr -> Bool
prop_ite expr = eliminateITE2 expr == eliminateITE expr

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

distributionsNOT :: Expr -> [(Expr, Expr)]
distributionsNOT (Eite _ _ _) = error "distributionsNOT doesn't work on Eite!"
distributionsNOT (Eiff _)     = error "distributionsNOT doesn't work on Eiff!"
distributionsNOT (Eimp _ _)   = error "distributionsNOT doesn't work on Eimp!"
distributionsNOT e@(Enot (Enot se)) = (e, se) : distributionsNOT se
distributionsNOT e@(Enot Etrue) = [(e, Efalse)]
distributionsNOT e@(Enot Efalse) = [(e, Etrue)]
distributionsNOT e@(Enot (Eand ses)) = (e, Eor  $ map Enot ses) : concatMap (distributionsNOT . Enot) ses
distributionsNOT e@(Enot (Eor  ses)) = (e, Eand $ map Enot ses) : concatMap (distributionsNOT . Enot) ses
distributionsNOT (Eor  ses) = concatMap distributionsNOT ses
distributionsNOT (Eand ses) = concatMap distributionsNOT ses
distributionsNOT (Exor ses) = concatMap distributionsNOT ses
distributionsNOT _ = []  -- (Enot (Exor _)), Esym, (Enot (Esym _)), Etrue, Efalse

distributeNOT :: Expr -> Expr
distributeNOT (Eite _ _ _) = error "distributeNOT doesn't work on Eite!"
distributeNOT (Eiff _)     = error "distributeNOT doesn't work on Eiff!"
distributeNOT (Eimp _ _)   = error "distributeNOT doesn't work on Eimp!"
distributeNOT (Enot (Enot se)) = distributeNOT se
distributeNOT (Enot Etrue) = Efalse
distributeNOT (Enot Efalse) = Etrue
distributeNOT (Enot (Eand ses)) = Eor  $ map (distributeNOT . Enot) ses
distributeNOT (Enot (Eor  ses)) = Eand $ map (distributeNOT . Enot) ses
distributeNOT (Eand ses) = Eand $ map distributeNOT ses
distributeNOT (Eor ses)  = Eor  $ map distributeNOT ses
distributeNOT (Exor ses) = Exor $ map distributeNOT ses
distributeNOT e@_ = e  -- (Enot (Exor _)), Esym, (Enot (Esym _)), Etrue, Efalse

{- eliminateITE eliminates all if-then-else expressions of form Γ ? Δ : Ω in the
given expression by replacing them with (Γ ^ Δ) v (!Γ ^ Ω).
-}
eliminateITE :: Expr -> Expr
eliminateITE (Enot se) = Enot $ eliminateITE se
eliminateITE (Eimp cond cons) = Eimp (eliminateITE cond) (eliminateITE cons)
eliminateITE (Eite cond cons alt) = Eor [Eand [eliminateITE cond, eliminateITE cons], Eand [Enot $ eliminateITE cond, eliminateITE alt]]
eliminateITE (Eand ses) = Eand $ map eliminateITE ses
eliminateITE (Eor ses)  = Eor  $ map eliminateITE ses
eliminateITE (Exor ses) = Exor $ map eliminateITE ses
eliminateITE (Eiff ses) = Eiff $ map eliminateITE ses
eliminateITE e@_ = e  -- Esym, Etrue, Efalse

eliminationsITE :: Expr -> [(Expr, Expr)]
eliminationsITE = nub . recurse
    where
        recurse :: Expr -> [(Expr, Expr)]
        recurse (Enot se) = recurse se
        recurse (Eimp cond cons) = recurse cond ++ recurse cons
        recurse e@(Eite cond cons alt) =
            (e, Eor [Eand [cond, cons], Eand [Enot cond, alt]]) : recurse cond ++ recurse cons ++ recurse alt
        recurse (Eand ses) = concatMap recurse ses
        recurse (Eor ses)  = concatMap recurse ses
        recurse (Exor ses) = concatMap recurse ses
        recurse (Eiff ses) = concatMap recurse ses
        recurse _ = []

{- eliminateIFF eliminates all if-and-only-if expressions of form
Γ1 <=> Γ2 <=> ... <=> Γn in the given expression by replacing them with
!(Γ1 + Γ2 + ... + Γn)
-}
eliminateIFF :: Expr -> Expr
eliminateIFF (Enot se) = Enot $ eliminateIFF se
eliminateIFF (Eimp cond cons) = Eimp (eliminateIFF cond) (eliminateIFF cons)
eliminateIFF (Eite cond cons alt) = Eite (eliminateIFF cond) (eliminateIFF cons) (eliminateIFF alt)
eliminateIFF (Eand ses) = Eand $ map eliminateIFF ses
eliminateIFF (Eor ses)  = Eor  $ map eliminateIFF ses
eliminateIFF (Exor ses) = Exor $ map eliminateIFF ses
eliminateIFF (Eiff ses) = Enot $ Exor $ map eliminateIFF ses
eliminateIFF e@_ = e  -- Esym, Etrue, Efalse

eliminationsIFF :: Expr -> [(Expr, Expr)]
eliminationsIFF = nub . recurse
    where
        recurse :: Expr -> [(Expr, Expr)]
        recurse (Enot se) = recurse se
        recurse (Eimp cond cons) = recurse cond ++ recurse cons
        recurse (Eite cond cons alt) = recurse cond ++ recurse cons ++ recurse alt
        recurse (Eand ses) = concatMap recurse ses
        recurse (Eor ses)  = concatMap recurse ses
        recurse (Exor ses) = concatMap recurse ses
        recurse e@(Eiff ses) = (e, Enot $ Exor ses) : concatMap recurse ses
        recurse _ = []

eliminateIMP :: Expr -> Expr
eliminateIMP (Enot se) = Enot $ eliminateIMP se
eliminateIMP (Eimp cond cons) = Eor [Enot (eliminateIMP cond), eliminateIMP cons]
eliminateIMP (Eite cond cons alt) = Eite (eliminateIMP cond) (eliminateIMP cons) (eliminateIMP alt)
eliminateIMP (Eand ses) = Eand $ map eliminateIMP ses
eliminateIMP (Eor ses)  = Eor  $ map eliminateIMP ses
eliminateIMP (Exor ses) = Exor $ map eliminateIMP ses
eliminateIMP (Eiff ses) = Eiff $ map eliminateIMP ses
eliminateIMP e@_ = e  -- Esym, Etrue, Efalse

eliminationsIMP :: Expr -> [(Expr, Expr)]
eliminationsIMP = nub . recurse
    where
        recurse :: Expr -> [(Expr, Expr)]
        recurse (Enot se) = recurse se
        recurse e@(Eimp cond cons) = (e, Eor [Enot (eliminateIMP cond), eliminateIMP cons]) : recurse cond ++ recurse cons
        recurse (Eite cond cons alt) = recurse cond ++ recurse cons ++ recurse alt
        recurse (Eand ses) = concatMap recurse ses
        recurse (Eor ses)  = concatMap recurse ses
        recurse (Exor ses) = concatMap recurse ses
        recurse (Eiff ses) = concatMap recurse ses
        recurse _ = []

{- eliminateXORdnf eliminates all XOR expressions of form Γ1 + Γ2 + ... + Γn in
the given expression by replacing them with OR-of all possible combinations of n
of Γ1, Γ2, ..., Γn AND'd where in each combination an even number of Γs are
negated (i.e. 0, 2, 4, ...).

For instance, for (Γ1 + Γ2 + Γ3 + Γ4 + Γ5):
  ( Γ1 +  Γ2 +  Γ3 +  Γ4 +  Γ5)
≡
  ( Γ1 ^  Γ2 ^  Γ3 ^  Γ4 ^  Γ5)
                                  number of negated Γs in each AND expression: 0

v (!Γ1 ^ !Γ2 ^  Γ3 ^  Γ4 ^  Γ5) v (!Γ1 ^  Γ2 ^ !Γ3 ^  Γ4 ^  Γ5)
v (!Γ1 ^  Γ2 ^  Γ3 ^ !Γ4 ^  Γ5) v (!Γ1 ^  Γ2 ^  Γ3 ^  Γ4 ^ !Γ5)
v ( Γ1 ^ !Γ2 ^ !Γ3 ^  Γ4 ^  Γ5) v ( Γ1 ^ !Γ2 ^  Γ3 ^ !Γ4 ^  Γ5)
v ( Γ1 ^ !Γ2 ^  Γ3 ^  Γ4 ^ !Γ5) v ( Γ1 ^  Γ2 ^ !Γ3 ^ !Γ4 ^  Γ5)
v ( Γ1 ^  Γ2 ^ !Γ3 ^  Γ4 ^ !Γ5) v ( Γ1 ^  Γ2 ^  Γ3 ^ !Γ4 ^ !Γ5)
                                  number of negated Γs in each AND expression: 2

v (!Γ1 ^ !Γ2 ^ !Γ3 ^ !Γ4 ^  Γ5) v (!Γ1 ^ !Γ2 ^ !Γ3 ^  Γ4 ^ !Γ5)
v (!Γ1 ^ !Γ2 ^  Γ3 ^ !Γ4 ^ !Γ5) v (!Γ1 ^  Γ2 ^ !Γ3 ^ !Γ4 ^ !Γ5)
v ( Γ1 ^ !Γ2 ^ !Γ3 ^ !Γ4 ^ !Γ5)
                                  number of negated Γs in each AND expression: 4

Total number of AND expressions: C(5, 0) + C(5, 2) + C(5, 4) = 1 + 10 + 5 = 16
-}
eliminateXORdnf :: Expr -> Expr
eliminateXORdnf (Enot se) = Enot $ eliminateXORdnf se
eliminateXORdnf (Eimp cond cons) = Eimp (eliminateXORdnf cond) (eliminateXORdnf cons)
eliminateXORdnf (Eite cond cons alt) = Eite (eliminateXORdnf cond) (eliminateXORdnf cons) (eliminateXORdnf alt)
eliminateXORdnf (Eand ses) = Eand $ map eliminateXORdnf ses
eliminateXORdnf (Eor ses)  = Eor  $ map eliminateXORdnf ses
eliminateXORdnf (Exor ses) =
    let l = map eliminateXORdnf ses
    in  Eor $ concatMap (\e -> map (\combination -> Eand $ negateIn l combination) $ combinations l e) (if length l `mod` 2 == 0 then [1,3..length l] else [0,2..length l])
    where
        negateIn :: [Expr] -> [Expr] -> [Expr]
        negateIn (x:xs) neg =
            if x `elem` neg then
                Enot x : negateIn xs neg
            else
                x : negateIn xs neg
        negateIn [] _ = []
eliminateXORdnf (Eiff ses) = Eiff $ map eliminateXORdnf ses
eliminateXORdnf e@_ = e  -- Esym, Etrue, Efalse

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
