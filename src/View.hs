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
import System.IO
import System.Process

import Expression

showSET :: SET -> String
showSET = recurse 0
    where
        recurse :: Int -> SET -> String
        recurse level (SET expr []) = show expr
        recurse level (SET expr sets) =
            let indent      = (concat $ replicate level "│  ")
                linePrefix  = indent ++ "├─ "
                nl          = '\n' : linePrefix
            in  show expr ++ nl ++ (foldr1 (\a b -> a ++ nl ++ b) $ map (recurse $ level + 1) sets)

viewSubexpressions :: SET -> String
viewSubexpressions set =
       bold "Sub-Expression Tree:\n"
    ++ concatMap (\l -> "  " ++ l ++ "\n") (lines $ showSET set)
    ++ "\n\n"
    ++ bold "Sub-Expression List:\n"
    ++ prettifyList (map show $ flatten set)

viewSymbols :: [Expr] -> String
viewSymbols ss =
       bold "Symbols:\n"
    ++ prettifyList (map (\(Esym s) -> s) ss)

viewCNF :: [([(Expr, Expr)], Expr)] -> String
viewCNF ts =
        bold "First eliminate ITE:\n"
     ++ prettifyList (map showPair $ fst (ts !! 0))
     ++ "After all:\n    " ++ show (snd $ ts !! 0)
     ++ "\n\n"
     ++ bold "Then eliminate IFF:\n"
     ++ prettifyList (map showPair $ fst (ts !! 1))
     ++ "After all:\n    " ++ show (snd $ ts !! 1)
     ++ "\n\n"
     ++ bold "Then eliminate IMP:\n"
     ++ prettifyList (map showPair $ fst (ts !! 2))
     ++ "After all:\n    " ++ show (snd $ ts !! 2)
     ++ "\n\n"
     ++ bold "Then distribute NOTs:\n"
     ++ prettifyList (map showPair $ fst (ts !! 3))
     ++ "After all:\n    " ++ show (snd $ ts !! 3)
     ++ "\n\n"
     ++ bold "eliminate subexpressions of form (Enot $ Exor _):\n"
     ++ prettifyList (map showPair $ fst (ts !! 4))
     ++ "After all:\n    " ++ show (snd $ ts !! 4)
     ++ "\n\n"
     ++ bold "eliminate subexpressions of form (Exor _):\n"
     ++ prettifyList (map showPair $ fst (ts !! 5))
     ++ "After all:\n    " ++ show (snd $ ts !! 5)
     ++ "\n\n"
     ++ bold "Then distribute NOTs once again:\n"
     ++ prettifyList (map showPair $ fst (ts !! 6))
     ++ "After all:\n    " ++ show (snd $ ts !! 6)
     ++ "\n\n"
     ++ bold "Distribute OR over AND\n"
     ++ prettifyList (map showPair $ fst (ts !! 7))
     ++ "After all:\n    " ++ show (snd $ ts !! 7)
     ++ "\n"

viewDNF :: [([(Expr, Expr)], Expr)] -> String
viewDNF ts =
       bold "First eliminate ITE:\n"
    ++ prettifyList (map showPair $ fst (ts !! 0))
    ++ "After all:\n    " ++ show (snd $ ts !! 0)
    ++ "\n\n"
    ++ bold "Then eliminate IFF:\n"
    ++ prettifyList (map showPair $ fst (ts !! 1))
    ++ "After all:\n    " ++ show (snd $ ts !! 1)
    ++ "\n\n"
    ++ bold "Then eliminate IMP:\n"
    ++ prettifyList (map showPair $ fst (ts !! 2))
    ++ "After all:\n    " ++ show (snd $ ts !! 2)
    ++ "\n\n"
    ++ bold "Then distribute NOTs:\n"
    ++ prettifyList (map showPair $ fst (ts !! 3))
    ++ "After all:\n    " ++ show (snd $ ts !! 3)
    ++ "\n\n"
    ++ bold "eliminate subexpressions of form (Enot $ Exor _):\n"
    ++ prettifyList (map showPair $ fst (ts !! 4))
    ++ "After all:\n    " ++ show (snd $ ts !! 4)
    ++ "\n\n"
    ++ bold "eliminate subexpressions of form (Exor _):\n"
    ++ prettifyList (map showPair $ fst (ts !! 5))
    ++ "After all:\n    " ++ show (snd $ ts !! 5)
    ++ "\n\n"
    ++ bold "Then distribute NOTs once again:\n"
    ++ prettifyList (map showPair $ fst (ts !! 6))
    ++ "After all:\n    " ++ show (snd $ ts !! 6)
    ++ "\n\n"
    ++ bold "Distribute AND over OR\n"
    ++ prettifyList (map showPair $ fst (ts !! 7))
    ++ "After all:\n    " ++ show (snd $ ts !! 7)
    ++ "\n"

showPair :: (Expr, Expr) -> String
showPair (orig, new) = show orig ++ "\nis transformed into\n" ++ show new

viewLess :: String -> IO ()
viewLess str = callCommand $ "printf \"" ++ escape str ++ "\"| less -R~KN "
    where
        escape :: String -> String
        escape s = concat
            [
                case c of
                    '\\' -> "\\\\"
                    '"' -> "\\\""
                    _ -> [c]
            | c <- s]

prettifyList :: [String] -> String
prettifyList = concatMap (\x -> "  • " ++ foldr1 (\l r -> l ++ '\n' : replicate 4 ' ' ++ r) (splitOn "\n" x) ++ "\n")

bold :: String -> String
bold s = "\x1b[1m" ++ s ++ "\x1b[0m"
