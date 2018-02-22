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
module View where

import Data.List.Split
import Debug.Trace
import System.IO
import System.Process

import DataTypes

printHeader :: String -> String
printHeader str = str ++ "\n" ++ replicate (visualLength str) '━'
    where
        {- visualLength calculates the "visual length" of a given string, by
        ignoring the terminal escape sequences.
        -}
        visualLength :: String -> Int
        visualLength str = length str - 4 * '\x1b' `countIn` str

        countIn :: Eq a => a -> [a] -> Int
        countIn t = length . filter (== t)

viewEntailment :: EntailmentResult -> String
viewEntailment res = undefined

viewTabulate :: Expr -> ([Expr], [[Bool]]) -> String
viewTabulate expr (headers, rows) =
    let
        {- BEWARE: The assumption is that headers will ALWAYS be longer than
        the columns, which is always either ⊤ or ⊥.
        -}
        headers'   = map show headers
        colLengths = map length headers'
    in             printHeader(bold "tabulate" ++ " " ++ underline (show expr))
        ++ "\n"
        ++ "\n" ++ "╔" ++ foldr1 (\a b -> a ++ "╤" ++ b) (map (`replicate` '═') colLengths) ++ "╗"
        ++ "\n" ++ "║" ++ foldr1 (\a b -> a ++ "│" ++ b) (map bold headers')                ++ "║"
        ++ "\n" ++ "╟" ++ foldr1 (\a b -> a ++ "┼" ++ b) (map (`replicate` '─') colLengths) ++ "╢"
        ++ "\n" ++ foldr1 (\a b -> a ++ "\n" ++ b) (forEach (zip [0..] rows) (\(i, row) ->
            "║" ++
                (if i `mod` 2 == 1 then reverseText else id)
                (foldr1 (\a b -> a ++ "│" ++ b) (zipWith (\len cell -> center len (showCell cell)) colLengths row))
            ++ "║"
        ))
        ++ "\n" ++ "╚" ++ foldr1 (\a b -> a ++ "╧" ++ b) (map (`replicate` '═') colLengths) ++ "╝"

    where
        forEach :: [a] -> (a -> b) -> [b]
        forEach a f = map f a

        showCell :: Bool -> String
        showCell True  = "⊤"
        showCell False = "⊥"

        center :: Int -> String -> String
        center len str
            | length str <= len = let leftPadLen  = (len - length str) `div` 2
                                      rightPadLen = (len - length str) - leftPadLen
                                  in  replicate leftPadLen ' ' ++ str ++ replicate rightPadLen ' '
            | otherwise = error "str is longer than len!"

viewResolution :: Resolution -> String
viewResolution res =    bold "Resolution:"
                     ++ "\n"
                     ++ indent 4 (prettifyList (map (pClause $ clauseStatuses res) (initialStep res)))
                     ++ "\n"
                     ++ prettifyList (map (uncurry $ ff $ clauseStatuses res) $ resolutionSteps res)
                     ++ "\n"
    where
        ff :: [(Clause, ClauseStatus)] -> Resolvent -> Step -> String
        ff dict resolvent step = bold ("──┤ " ++ show resolvent ++ " ├────────────\n")
                                 ++ prettifyList (map (pClause dict) step)

        indent :: Int -> String -> String
        indent i s = foldr1 (\l r -> l ++ '\n' : r) (map (replicate i ' ' ++) (init $ splitOn "\n" s)) ++ "\n"

        -- TODO
        --    5> resolve (A ^ B ^ C ? D : A v B)
        -- boolexman: src/View.hs:42:55-108: Irrefutable pattern failed for pattern [(_, status)]
        pClause :: [(Clause, ClauseStatus)] -> Clause -> String
        pClause dict clause = case dict <!?> clause of
            Just (ResolvedBy resolvent) -> show resolvent ++ show clause
            Just Striken -> "~" ++ strike (show clause) ++ "~"
            Nothing -> show clause

showSET :: SET -> String
showSET = recurse 0
    where
        recurse :: Int -> SET -> String
        recurse level (SET expr []) = show expr
        recurse level (SET expr sets) =
            let indent      = (concat $ replicate level "│  ")
                linePrefix  = indent ++ "├─ "
                nl          = '\n' : linePrefix
            in  show expr ++ nl ++ foldr1 (\a b -> a ++ nl ++ b) (map (recurse $ level + 1) sets)

viewSubexpressions :: Expr -> SubexpressionsResult -> String
viewSubexpressions expr res =
               printHeader (bold "subexpressions" ++ " " ++ underline (show expr))
    ++ "\n"
    ++ "\n" ++ bold "Sub-Expression Tree:"
    ++ "\n" ++ concatMap (\l -> "  " ++ l ++ "\n") (lines $ showSET $ set res)
    ++ "\n"
    ++ "\n" ++ bold "Sub-Expression List:"
    ++ "\n" ++ prettifyList (map show $ list res)

viewSymbols :: Expr -> [Expr] -> String
viewSymbols expr ss =
               printHeader (bold "symbols" ++ " " ++ underline (show expr))
    ++ "\n"
    ++ "\n" ++ prettifyList (map (\(Esym s) -> s) ss)

viewXNF :: String -> String -> [([(Expr, Expr)], Expr)] -> String
viewXNF sixthStep sixthStep' ts =
    bold "1. Transform all if-then-else (ITE) expressions:\n"
 ++ (
        if   null $ fst $ ts !! 0
        then "  No ITE expressions are found!\n"
        else prettifyList (map showPair $ fst (ts !! 0))
 )
 ++ "\n" ++ bold "After all ITE expressions are transformed:\n  " ++ show (snd $ ts !! 0)
 ++ "\n\n"
 ++ bold "2. Transform all if-and-only-if (IFF) expressions:\n"
 ++ (
        if   null $ fst $ ts !! 1
        then "  No IFF expressions are found!\n"
        else prettifyList (map showPair $ fst (ts !! 1))
 )
 ++ "\n" ++ bold "After all IFF expressions are transformed:\n  " ++ show (snd $ ts !! 1)
 ++ "\n\n"
 ++ bold "3. Tranform all implications:\n"
 ++ (
        if   null $ fst $ ts !! 2
        then "  No implications are found!\n"
        else prettifyList (map showPair $ fst (ts !! 2))
 )
 ++ "\n" ++ bold "After all implications are transformed:\n  " ++ show (snd $ ts !! 2)
 ++ "\n\n"
 ++ bold "4. Tranform all exclusive-or (XOR) expressions:\n"
 ++ (
        if   null $ fst $ ts !! 3
        then "  No XOR expressions are found!\n"
        else prettifyList (map showPair $ fst (ts !! 3))
 )
 ++ "\n" ++ bold "After all XOR expressions are transformed:\n  " ++ show (snd $ ts !! 3)
 ++ "\n\n"
 ++ bold "5. Distribute NOTs:\n"
 ++ (
        if   null $ fst $ ts !! 4
        then "  No distributable NOTs are found!\n"
        else prettifyList (map showPair $ fst (ts !! 4))
 )
 ++ "\n" ++ bold "After all NOTs are distributed:\n  " ++ show (snd $ ts !! 4)
 ++ "\n\n"
 ++ bold ("6. " ++ sixthStep ++ ":\n")
 ++ (
        if   null $ fst $ ts !! 5
        then "  " ++ sixthStep' ++ "\n"
        else prettifyList (map showPair $ fst (ts !! 5))
 )
 ++ "\n" ++ bold "Resultant expression:\n  " ++ show (snd $ ts !! 5)
 ++ "\n"


viewCNF :: Expr -> [([(Expr, Expr)], Expr)] -> String
viewCNF expr ts =
               printHeader (bold "toCNF" ++ " " ++ underline (show expr))
    ++ "\n"
    ++ "\n" ++ viewXNF "Distribute ORs over ANDs" "No distributable OR statements are found!" ts

viewDNF :: Expr -> [([(Expr, Expr)], Expr)] -> String
viewDNF expr ts =
               printHeader (bold "toDNF" ++ " " ++ underline (show expr))
    ++ "\n"
    ++ "\n" ++ viewXNF "Distribute ANDs over ORs" "No distributable AND statements are found!" ts

viewEval :: [Expr] -> [Expr] -> Expr -> EvalResult -> String
viewEval ts fs expr r =
        printHeader (bold "eval" ++ " " ++ underline (show ts) ++ " " ++ underline (show fs) ++ " " ++ underline (show expr))
     ++ "\n"
     ++ "\n" ++ (
        if   not (null (redundantTrueSymbols r)) || not (null (redundantFalseSymbols r))
        then    bold "WARNING: Some of the true/false symbols have not been found in the expression!\n"
             ++ (if not (null (redundantTrueSymbols  r)) then "  Redundant True Symbols : " ++ show (redundantTrueSymbols  r) else "")
             ++ (if not (null (redundantFalseSymbols r)) then "\n  Redundant False Symbols: " ++ show (redundantFalseSymbols r) else "")
             ++ "\n\n"
        else "")
     ++ bold "1. Transform into Conjunctive Normal Form:" ++ "\n"
     ++ prettifyList (map showClause $ productOfSums r) ++ "\n"
     ++ bold "2. Eliminate all maxterms which constains a true symbol:" ++ "\n"
     ++ (
        if null $ trueEliminations r
        then "  No maxterms are eliminated.\n"
        else prettifyList (map (showPair2 "true") $ trueEliminations r)
     ) ++ "\n"
     ++ bold "Remaining maxterms:" ++ "\n"
     ++ prettifyList (map showClause $ postTrueElimination r) ++ "\n"
     ++ bold "3. Transform into Disjunctive Normal Form:" ++ "\n"
     ++ prettifyList (map showClause $ sumOfProducts r) ++ "\n"
     ++ bold "4. Eliminate all minterms which constains a false symbol:" ++ "\n"
     ++ (
        if   null $ falseEliminations r
        then "  No minterms are eliminated.\n"
        else prettifyList (map (showPair2 "false") $ falseEliminations r)
     ) ++ "\n"
     ++ bold "Remaining minterms:" ++ "\n"
     ++ prettifyList (map showClause $ postFalseElimination r)
     ++ "\n" ++ bold "Resultant expression:"
     ++ "\n" ++ "  " ++ show (result r)
    where
        showClause :: [Expr] -> String
        showClause [] = "{}"
        showClause xs = "{" ++ foldr1 (\a b -> a ++ ", " ++ b) (map show xs) ++ "}"

        showPair2 :: String -> (Expr, [Expr]) -> String
        showPair2 tf (sym, term) = showClause term ++ "\nis eliminated because " ++ show sym ++ " is " ++ tf ++ "."

showPair :: (Expr, Expr) -> String
showPair (orig, new) = show orig ++ "\nis transformed into\n" ++ show new

viewLess2 :: String -> IO ()
viewLess2 str = callCommand $ "printf \"" ++ escape str ++ "\"| less -R~KNS "
    where
        escape :: String -> String
        escape s = concat
            [
                case c of
                    '\\' -> "\\\\"
                    '"' -> "\\\""
                    _ -> [c]
            | c <- s]

viewLess3 :: String -> IO()
viewLess3 str = do
    (Just lessStdin, _, _, _) <- createProcess (proc "less" []) { std_in  = CreatePipe
                                                                , std_out = Inherit
                                                                }
    hPutStr lessStdin str
    hFlush lessStdin

prettifyList :: [String] -> String
prettifyList = concatMap (\x -> "  " ++ bold "•" ++ " " ++ foldr1 (\l r -> l ++ '\n' : replicate 4 ' ' ++ r) (splitOn "\n" x) ++ "\n")

bold :: String -> String
bold s = "\x1b[1m" ++ s ++ "\x1b[0m"

strike :: String -> String
strike s = "\x1b[9m" ++ s ++ "\x1b[0m"

underline :: String -> String
underline s = "\x1b[4m" ++ s ++ "\x1b[0m"

reverseText :: String -> String
reverseText s = "\x1b[7m" ++ s ++ "\x1b[0m"
