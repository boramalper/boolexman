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
module ViewProps (props) where
{-| Module `ViewTest` hosts (property) tests for view* functions in the `View`
module. These tests are hosted here, rather than `View`, due to their dependency
on lots of other modules (e.g. `Engine.Command`, `DataTypes`); they can also
serve as (improper!) integration tests.

The tests are annoyingly simple, and might even seem pointless: the whole point
of them is to make sure we won't encounter anything like
`*** Exception: Prelude.foldr1: empty list`.
-}
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, discard)

import DataTypes
import Engine.Commands
import View
import Utils (countIn)

props :: TestTree
props = testGroup "View"
    [ testProperty "prop_viewEntailment"     prop_viewEntailment
    , testProperty "prop_viewTabulate"       prop_viewTabulate
    , testProperty "prop_viewResolution"     prop_viewResolution
    , testProperty "prop_viewSubexpressions" prop_viewResolution
    , testProperty "prop_viewSymbols"        prop_viewSymbols
    , testProperty "prop_viewCNF"            prop_viewCNF
    , testProperty "prop_viewDNF"            prop_viewDNF
    , testProperty "prop_viewEval"           prop_viewEval
    ]

prop_viewEntailment :: Expr -> Expr -> Bool
prop_viewEntailment cond expr
    | all (not . (`subexprOf` cond)) [Etrue, Efalse] && all (not . (`subexprOf` expr)) [Etrue, Efalse] =
        let res = viewEntailment cond expr $ entail cond expr
        in  "\n\n" `countIn` res <= 1
    | otherwise = discard

prop_viewTabulate :: Expr -> Bool
prop_viewTabulate expr =
    let res = viewTabulate expr $ tabulate expr
    in  "\n\n" `countIn` res == 1

prop_viewResolution :: Expr -> Bool
prop_viewResolution expr =
    let res = viewResolution expr $ resolve expr
    in  "\n\n" `countIn` res == 1

prop_viewSubexpressions :: Expr -> Bool
prop_viewSubexpressions expr =
    let res = viewSubexpressions expr $ subexpressions expr
    in  "\n\n" `countIn` res == 2

prop_viewSymbols :: Expr -> Bool
prop_viewSymbols expr =
    let res = viewSymbols expr $ symbols expr
    in  "\n\n" `countIn` res == 1

prop_viewCNF :: Expr -> Bool
prop_viewCNF expr =
    let res = viewCNF expr $ toCNF expr
    in  "\n\n" `countIn` res == 1

prop_viewDNF :: Expr -> Bool
prop_viewDNF expr =
    let res = viewDNF expr $ toDNF expr
    in  "\n\n" `countIn` res == 1

prop_viewEval :: [Expr] -> [Expr] -> Expr -> Bool
prop_viewEval trueSymbols falseSymbols expr =
    let res = viewEval trueSymbols falseSymbols expr $ eval trueSymbols falseSymbols expr
    in  "\n\n" `countIn` res == 1
