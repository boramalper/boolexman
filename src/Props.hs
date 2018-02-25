module Main where

import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified ViewProps

main :: IO ()
main = defaultMain $ localOption (mkTimeout $ 1 * 1000000) props

props :: TestTree
props = testGroup "boolexman"
    [ ViewProps.props
    ]
