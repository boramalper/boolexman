{- boolexman -- boolean expression manipulator
Copyright (c) 2018 Mert Bora ALPER <bora@boramalper.org>

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
module ParserProps (props) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, discard)

import DataTypes (Expr)
import Parser (locateFirst, parse)

props :: TestTree
props = testGroup "Parser"
    [ testProperty "prop_locateFirst" prop_locateFirst
    , testProperty "prop_parse"       prop_parse
    ]

prop_locateFirst :: [String] -> String -> Bool
prop_locateFirst subs str = case locateFirst subs str of
    Nothing  -> discard
    Just (prefix, match, suffix) -> prefix ++ match ++ suffix == str

{- show Expr is generous with parantheses and therefore this test might be too
merciful on some edge cases relating to operator precedence, but hey, it's still
better than nothing!
-}
prop_parse :: Expr -> Bool
prop_parse expr = case parse $ show expr of
    Left s      -> error s
    Right expr' -> expr == expr'
