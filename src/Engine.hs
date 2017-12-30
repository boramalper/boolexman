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
symbols s@(Esym sym) = [s]
symbols _  = []  -- Etrue, Efalse

{- eliminateITE eliminates all if-then-else expressions of form Γ ? Δ : Ω in the
given expression by replacing them with (Γ ^ Δ) v (!Γ ^ Ω).
-}
eliminateITE :: Expr -> Expr
eliminateITE (Enot se) = eliminateITE se
eliminateITE (Eimp cond cons) = Eimp (eliminateITE cond) (eliminateITE cons)
eliminateITE (Eite cond cons alt) = Eor [Eand [cond, cons], Eand [Enot cond, alt]]
eliminateITE (Eand ses) = Eand $ map eliminateITE ses
eliminateITE (Eor ses)  = Eor  $ map eliminateITE ses
eliminateITE (Exor ses) = Exor $ map eliminateITE ses
eliminateITE (Eiff ses) = Eiff $ map eliminateITE ses
eliminateITE e@_ = e  -- Esym, Etrue, Efalse

{- eliminateIFF eliminates all if-and-only-if expressions of form
Γ1 <=> Γ2 <=> ... <=> Γn in the given expression by replacing them with
!(Γ1 + Γ2 + ... + Γn)
-}
eliminateIFF :: Expr -> Expr
eliminateIFF (Enot se) = eliminateIFF se
eliminateIFF (Eimp cond cons) = Eimp (eliminateIFF cond) (eliminateIFF cons)
eliminateIFF (Eite cond cons alt) = Eite (eliminateIFF cond) (eliminateIFF cons) (eliminateIFF alt)
eliminateIFF (Eand ses) = Eand $ map eliminateIFF ses
eliminateIFF (Eor ses)  = Eor  $ map eliminateIFF ses
eliminateIFF (Exor ses) = Exor $ map eliminateIFF ses
eliminateIFF (Eiff ses) = Enot $ Exor $ map eliminateIFF ses
eliminateIFF e@_ = e  -- Esym, Etrue, Efalse

eliminateIMP :: Expr -> Expr
eliminateIMP (Enot se) = eliminateIMP se
eliminateIMP (Eimp cond cons) = Eor [Enot (eliminateIMP cond), eliminateIMP cons]
eliminateIMP (Eite cond cons alt) = Eite (eliminateIMP cond) (eliminateIMP cons) (eliminateIMP alt)
eliminateIMP (Eand ses) = Eand $ map eliminateIMP ses
eliminateIMP (Eor ses)  = Eor  $ map eliminateIMP ses
eliminateIMP (Exor ses) = Exor $ map eliminateIMP ses
eliminateIMP (Eiff ses) = Eiff $ map eliminateIMP ses
eliminateIMP e@_ = e  -- Esym, Etrue, Efalse

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
eliminateXORdnf (Enot se) = eliminateXORdnf se
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
