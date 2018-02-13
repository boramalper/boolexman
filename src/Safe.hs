module Safe where

import GHC.Stack

head :: [a] -> a
head (x:xs) = x
head _      = errorWithStackTrace "sad sad sad"
