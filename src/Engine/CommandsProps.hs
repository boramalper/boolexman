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
module Engine.CommandsProps where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, discard)

import DataTypes
import Engine.Commands
import Engine.Other

props :: TestTree
props = testGroup "Commands"
    [ testProperty "prop_tabulate" prop_tabulate
    , testProperty "prop_toCNF"    prop_toCNF
    , testProperty "prop_toDNF"    prop_toDNF
    , testProperty "prop_eval"     prop_eval
    , testProperty "prop_entail"   prop_entail
    , testProperty "prop_resolve"  prop_resolve
    ]

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

prop_toCNF :: Expr -> Bool
prop_toCNF expr = let result = snd $ last $ toCNF expr
                  in  if   expr == result
                      then discard
                      else isCNF result && (expr == result || equivalent expr result)

prop_toDNF :: Expr -> Bool
prop_toDNF expr = let result = snd $ last $ toDNF expr
                  in  if   expr == result
                      then if isDNF result then discard else False
                      else isDNF result && equivalent expr result

prop_eval :: Expr -> Bool
prop_eval expr = all (\(ts, fs) -> postFalseElimination (eval ts fs expr) == toExpr (evalS ts fs expr)) $ evaluations expr
    where
        toExpr :: Bool -> [[Expr]]
        toExpr True  = [[Etrue]]
        toExpr False = [[Efalse]]

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
      doesEntail (Lor  _ subents) = all doesEntail subents
      doesEntail (Rand _ subents) = all doesEntail subents

prop_resolve :: Expr -> Bool
prop_resolve expr = satisfiable (resolve expr) == any (\(ts, fs) -> evalS ts fs expr) (evaluations expr)
    where
        satisfiable :: Resolution -> Bool
        satisfiable res = not ([[], [Efalse]] `elemN` (initialStep res) || any ([[], [Efalse]] `elemN`) (map snd (resolutionSteps res)))

        elemN :: Eq a => [a] -> [a] -> Bool
        elemN needles haystack = any (`elem` haystack) needles
