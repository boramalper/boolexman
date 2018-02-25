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
module Main where

import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.QuickCheck

import qualified EngineProps
import qualified ViewProps
import qualified UtilsProps
import qualified ParserProps

main :: IO ()
main = defaultMain
    -- 60 seconds PER property
    $ localOption (mkTimeout $ 60 * 1000000)
    -- 10,000 tests PER property.
    $ localOption (QuickCheckTests 10000)
    -- Try until timeout.
    $ localOption (QuickCheckMaxRatio 10000)
    props

props :: TestTree
props = testGroup "boolexman"
    [ EngineProps.props
    , ViewProps.props
    , UtilsProps.props
    , ParserProps.props
    ]
