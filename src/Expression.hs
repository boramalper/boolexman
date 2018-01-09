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
    deriving (Eq)

instance Show Expr where
    show (Enot se) = '!' : show se
    show (Eimp cond cons) = parens $ show cond ++ " => " ++ show cons
    show (Eite cond cons alt) = parens $ show cond ++ " ? " ++ show cons ++ " : " ++ show alt
    show (Eand es) = parens $ foldr1 (\a b -> a ++ " ^ " ++ b) $ map show es
    show (Eor es)  = parens $ foldr1 (\a b -> a ++ " v " ++ b) $ map show es
    show (Exor es) = parens $ foldr1 (\a b -> a ++ " + " ++ b) $ map show es
    show (Eiff es) = parens $ foldr1 (\a b -> a ++ " <=> " ++ b) $ map show es
    show (Esym sym) = sym
    show Etrue = "True"
    show Efalse = "False"

-- TODO: instance Arbitrary Expr where

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
