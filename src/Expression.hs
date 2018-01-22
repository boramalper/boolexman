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

import Control.Monad
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

isSymbol :: Expr -> Bool
isSymbol (Esym _) = True
isSymbol Etrue    = True
isSymbol Efalse   = True
isSymbol _ = False

isNegSymbol :: Expr -> Bool
isNegSymbol (Enot subexpr) = isSymbol subexpr
isNegSymbol _ = False

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
    arbitrary = sized prop
        where
            prop n | n <= 0     = atom
                   | otherwise  = oneof [ atom
                                        , liftM  Enot subform
                                        , liftM2 Eimp subform subform
                                        , liftM3 Eite subform' subform' subform'
                                        , liftM  Eand $ vectorOf 4 subform
                                        , liftM  Eor  $ vectorOf 4 subform
                                        , liftM  Exor $ vectorOf 4 subform
                                        , liftM  Eiff $ vectorOf 4 subform
                                        ]
                    where
                        atom = oneof [liftM Esym (elements ["P", "Q", "R", "S", "T"]),
                                        elements [Etrue, Efalse]]
                        subform  = prop (n `div` 8)
                        subform' = prop (n `div` 16)

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
