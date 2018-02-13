module Safe where

import GHC.Stack

head :: String -> [a] -> a
head s (x:xs) = x
head s _      = error $ "head failed! " ++ s
