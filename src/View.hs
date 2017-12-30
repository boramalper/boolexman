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
    ++ concat (map (\l -> "  " ++ l ++ "\n") (lines $ showSET set))
    ++ "\n\n"
    ++ bold "Sub-Expression List:\n"
    ++ (prettifyList $ map show $ flatten set)

viewSymbols :: [Expr] -> String
viewSymbols ss =
       bold "Symbols:\n"
    ++ prettifyList (map (\(Esym s) -> s) ss)

viewLess :: String -> IO ()
viewLess s = do
    callCommand $ "printf \"" ++ sanitize s ++ "\"| less -R~KN "
    where
        sanitize :: String -> String
        sanitize s = concat $
            [
                case c of
                    '\\' -> "\\\\"
                    '"' -> "\\\""
                    _ -> [c]
            | c <- s]

prettifyList :: [String] -> String
prettifyList xs = concat $ map (\x -> "  • " ++ x ++ "\n") xs

bold :: String -> String
bold s = "\x1b[1m" ++ s ++ "\x1b[0m"
