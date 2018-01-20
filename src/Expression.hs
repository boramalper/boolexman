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
module Expression where

import Data.List
import Debug.Trace
import Test.QuickCheck
import Test.QuickCheck.Arbitrary


data Expr = Enot Expr
          | Eimp Expr Expr
          | Eite Expr Expr Expr
          | Eand [Expr]
          | Eor  [Expr]
          | Exor [Expr]
          | Eiff [Expr]
          | Esym String
          | Etrue
          | Efalse
    deriving Eq

instance Show Expr where
    show (Enot se) = '!' : show se
    show (Eimp cond cons) = parens $ show cond ++ " => " ++ show cons
    show (Eite cond cons alt) = parens $ show cond ++ " ? " ++ show cons ++ " : " ++ show alt
    show (Eand es) = parens $ "" ++ (foldr1 (\a b -> a ++ " ^ " ++ b) $ map show es)
    show (Eor es)  = parens $ "" ++ (foldr1 (\a b -> a ++ " v " ++ b) $ map show es)
    show (Exor es) = parens $ "" ++ (foldr1 (\a b -> a ++ " + " ++ b) $ map show es)
    show (Eiff es) = parens $ "" ++ (foldr1 (\a b -> a ++ " <=> " ++ b) $ map show es)
    show (Esym sym) = sym
    show Etrue = "True"
    show Efalse = "False"

-- TODO: I don't think this is a "high quality" one...
instance Arbitrary Expr where
    arbitrary =
        let
            -- TODO: Isn't there a better way to indicate that x is an Int for
            -- God's sake!?
            definitelyInt :: Int -> Int
            definitelyInt x = x
        in do { x <- arbitrary;
              ; case definitelyInt x `mod` 6 of
                    0 -> do { subexpr <- arbitrary
                            ; return $ Enot subexpr }
                    1 -> do { cond <- arbitrary
                            ; cons <- arbitrary
                            ; return $ Eimp cond cons }
                    2 -> do { cond <- arbitrary
                            ; cons <- arbitrary
                            ; alt  <- arbitrary
                            ; return $ Eite cond cons alt }
                    3 -> do { subexprs <- arbitrary
                            ; return $ Eand subexprs }
                    4 -> do { subexprs <- arbitrary
                            ; return $ Eor subexprs }
                    5 -> do { subexprs <- arbitrary
                            ; return $ Exor subexprs }
                    6 -> do { subexprs <- arbitrary
                            ; return $ Eiff subexprs }
                    {- TODO: Why only A,B,C,D,E? We would like some symbols to
                    appear multiple times in an expression so that we can test
                    some more complicated cases.
                    -}
                    7 -> do { i <- arbitrary
                            ; return $ Esym $ ["A", "B", "C", "D", "E"] !! (i `mod` 5) }
                    8 -> return Etrue
                    9 -> return Efalse

                }
prop_deneme :: Expr -> Bool
prop_deneme expr = not $ null $ trace (show expr ++ "\n") $ show expr

-- SET: Sub-Expression Tree
data SET = SET Expr [SET] deriving Eq

flatten :: SET -> [Expr]
flatten (SET expr sets) = nub $ expr : concatMap flatten sets

data EvalResult = EvalResult { redundantTrueSymbols  :: [Expr]
                             , redundantFalseSymbols :: [Expr]
                             , cnf                   :: Expr
                             , trueEliminations      :: [(Expr, [Expr])]
                             , postTrueElimination   :: Expr
                             , dnf                   :: Expr
                             , falseEliminations     :: [(Expr, [Expr])]
                             , postFalseElimination  :: Expr
                             } deriving (Show)

parens :: String -> String
parens s = '(' : s ++ ")"
