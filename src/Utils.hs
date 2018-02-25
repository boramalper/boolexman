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
module Utils where
{-| Module `Utils` consists of general purpose utils. As a thumb of rule:

  1. If you feel that you need to "import" another module of `boolexman`,
     consider moving the function you are writing under that module, or create a
     completely new module.

     (This is implies that type signatures of the functions in this module
     should be as generic as possible.)
  2. Most of the functions here exist likely due to my ignorance of the standard
     library, and libraries other people have already developed & tested. You
     are more than welcome to remove functions from this module, and ideally
     remove the module `Utils` altogether.
-}

import Data.List (isPrefixOf)

findOne :: Eq a => [a] -> [a] -> Maybe a
findOne [] _ = Nothing
findOne (n:needles) haystack = if n `elem` haystack then Just n else findOne needles haystack

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct as bs = concatMap (\a -> map (\b -> (a, b)) bs) as

{-
EXAMPLE:
  > combine [['A', 'B', 'C'], ['1', '2']]
  ["A1","A2","B1","B2","C1","C2"]
-}
combine :: [[a]] -> [[a]]
combine (l:ls) = concatMap (\e -> map (\l2 -> e:l2) $ combine ls) l
combine [] = [[]]

combinations :: [a] -> Int -> [[a]]
combinations _  0 = [[]]  -- C(0, 0) = C(length s, 0) = 1,  ∀s
combinations [] _ = []  -- C(0, k) = 0,  k /= 0
combinations s 1 = map (: []) s
combinations s k
    | k >= 0 =
        concat $ map' (\h t -> (map (\l -> h : l) $ combinations t (k - 1)) ) s
    | otherwise = []  -- C(length s, k) = 0,  k < 0
    where
        map' :: (a -> [a] -> b) -> [a] -> [b]
        map' _ [_] = []
        map' f (x:xs) = f x xs : map' f xs

{- Counts the number of times a substring occurs in a string.

EXAMPLE:

  > count "a" "ananas"
  3

  > count "aa" "aaaa"
  2  -- in contrast to three!
-}
countIn :: Eq a => [a] -> [a] -> Int
countIn sub str
    | length str < length sub = 0
    | sub `isPrefixOf` str = 1 + sub `countIn` drop (length sub) str
    | otherwise = sub `countIn` tail str
