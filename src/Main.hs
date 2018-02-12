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
module Main where

import Control.Monad
import Data.List.Split (splitOn)
import System.IO
import System.Console.Readline
import Text.Regex.TDFA

import Engine
import Parser
import View


main :: IO ()
main = do
    putStrLn "boolexman - boolean expression manipulator | v0.1.0.0"
    loop 1


loop :: Integer -> IO ()
loop no = do
    line <- readline $ '\n' : formatNo 4 no ++ "> "
    case line of
        Nothing -> putStrLn "\nEOF received, quitting..."
        Just line -> unless (line == "quit") $ do
            addHistory line
            viewLess2 $ process line
            loop $ no + 1
    where
        formatNo :: Int -> Integer -> String
        formatNo minLength no =
            let noStr = show no
            in replicate (minLength - length noStr) ' ' ++ noStr

process :: String -> String
process s =
    let s'       = normalize s
        command  = head $ splitOn " " s'
        argument = drop (length command + 1) s'  -- +1 for the space character

        -- CAPTURES the expression enclosed in parantheses
        expressionCRE = "\\((.*)\\)"
        symbolRE     = "[A-Z][a-zA-Z0-9]*"
        -- CAPTURES the comma-separated list of symbols between the square
        -- parantheses
        -- symbolListCRE = "\\[(" ++ symbolRE ++ "(?: ?, ?" ++ symbolRE ++ ")*)\\]"
        symbolListCRE = "\\[(.*)\\]"
    in case command of
        "tabulate" ->
            let (_, expression, _) = argument =~ expressionCRE :: (String, String, String)
            in  if   null expression
                then    "Parsing Error: could not parse the argument! (make"
                     ++ "  sure you enclose the expression in parantheses)"
                else case parse argument of
                    Left err  -> "Parsing Error: " ++ err
                    Right exp -> "riiight"
        "subexpressions" ->
            let (_, expression, _) = argument =~ expressionCRE :: (String, String, String)
            in  if   null expression
                then    "Parsing Error: could not parse the argument! (make"
                     ++ "  sure you enclose the expression in parantheses)"
                else case parse argument of
                    Left err  -> "Parsing Error: " ++ err
                    Right exp -> viewSubexpressions $ subexpressions exp
        "symbols" ->
            let (_, expression, _) = argument =~ expressionCRE :: (String, String, String)
            in  if   null expression
                then    "Parsing Error: could not parse the argument! (make"
                     ++ "  sure you enclose the expression in parantheses)"
                else case parse argument of
                    Left err  -> "Parsing Error: " ++ err
                    Right exp -> viewSymbols $ symbols exp
        -- TODO: This looks really ugly, isn't there a neater way?
        "eval" -> -- eval [P, Q] [R, S, T] ((P and Q and R) or (S implies T))
            let (_, _, _, matches) = argument =~ (symbolListCRE ++ ' ' : symbolListCRE ++ ' ' : expressionCRE) :: (String, String, String, [String])
            in  if   length matches /= 3
                then "Parsing Error: supply two lists of symbols, and an expression!"
                else case parseCsSymbols $ matches !! 0 of
                    Left err          -> "Parsing Error: " ++ err ++ "(in the first list)"
                    Right trueSymbols -> case parseCsSymbols $ matches !! 1 of
                        Left err           -> "Parsing Error: " ++ err ++ "(in the second list)"
                        Right falseSymbols -> case parse $ matches !! 2 of
                            Left err  -> "Parsing Error: " ++ err ++ "(in the expression)"
                            Right exp -> viewEval $ eval trueSymbols falseSymbols exp -- "tS: " ++ show trueSymbols ++ "  fS: " ++ show falseSymbols ++ "  ex: " ++ show exp
                            --

        "toDNF" ->
            let (_, expression, _) = argument =~ expressionCRE :: (String, String, String)
            in  if   null expression
                then    "Parsing Error: could not parse the argument! (make"
                     ++ "  sure you enclose the expression in parantheses)"
                else case parse argument of
                    Left err  -> "Parsing Error: " ++ err
                    Right exp -> viewDNF $ toDNF exp
        "toCNF" ->
            let (_, expression, _) = argument =~ expressionCRE :: (String, String, String)
            in  if   null expression
                then    "Parsing Error: could not parse the argument! (make"
                     ++ "  sure you enclose the expression in parantheses)"
                else case parse argument of
                    Left err  -> "Parsing Error: " ++ err
                    Right exp -> viewCNF $ toCNF exp
        "resolve" ->
            let (_, expression, _) = argument =~ expressionCRE :: (String, String, String)
            in  if   null expression
                then    "Parsing Error: could not parse the argument! (make"
                     ++ "  sure you enclose the expression in parantheses)"
                else case parse argument of
                    Left err  -> "Parsing Error: " ++ err
                    Right exp -> viewResolution $ resolve exp
        "entail" -> -- ((A implies (B and Q)) and (B implies C)) (A implies C)  -- gentzen
            let (_, _, _, expressions) = argument =~ (expressionCRE ++ " " ++ expressionCRE)
                                         :: (String, String, String, [String])
            in  if   length expressions /= 2
                then    "Parsing Error: could not parse the argument! (make"
                     ++ "  sure you enclose the expressions in parantheses)"
                else case parseAll expressions of
                    Left err  -> "Parsing Error: " ++ err
                    Right exp -> show $ entail (exp !! 0) (exp !! 1)
        command ->
            "Error: Unknown command!"
