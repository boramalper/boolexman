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

import DataTypes
import Engine.Commands
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
            viewLess3 $ process line
            loop $ no + 1
    where
        formatNo :: Int -> Integer -> String
        formatNo minLength no =
            let noStr = show no
            in replicate (minLength - length noStr) ' ' ++ noStr

process :: String -> String
process s =
    let s'       = normaliseString s
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
        "tabulate" -> case parseSoleExpression argument of
            Left err   -> err
            Right expr -> viewTabulate expr $ tabulate expr
        "subexpressions" -> case parseSoleExpression argument of
            Left  err  -> err
            Right expr -> viewSubexpressions expr $ subexpressions expr
        "symbols" -> case parseSoleExpression argument of
            Left  err -> err
            Right expr -> viewSymbols expr $ symbols expr

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
                            Right expr -> viewEval trueSymbols falseSymbols expr $ eval trueSymbols falseSymbols expr -- "tS: " ++ show trueSymbols ++ "  fS: " ++ show falseSymbols ++ "  ex: " ++ show exp
        "toDNF" -> case parseSoleExpression argument of
            Left  err -> err
            Right expr -> viewDNF expr $ toDNF expr
        "toCNF" -> case parseSoleExpression argument of
            Left  err -> err
            Right expr -> viewCNF expr $ toCNF expr
        "resolve" -> case parseSoleExpression argument of
            Left  err -> err
            Right expr -> viewResolution expr $ resolve expr
        "entail" ->
            let (_, _, _, expressions) = argument =~ (expressionCRE ++ " " ++ expressionCRE)
                                         :: (String, String, String, [String])
            in  if   length expressions /= 2
                then    "Parsing Error: could not parse the argument! (make"
                     ++ "  sure you enclose the expressions in parantheses)"
                else case parseAll expressions of
                    Left err    -> "Parsing Error: " ++ err
                    Right [cond, expr] -> viewEntailment cond expr $ entail cond expr
        command ->
            "Error: Unknown command: " ++ command ++ "  "
    where
        parseSoleExpression :: String -> Either String Expr
        parseSoleExpression str =
            let (a, expression, b) = str =~ "\\((.*)\\)" :: (String, String, String)
            in  if   null expression || not (null a) || not (null b)
                then Left $    "Parsing Error: could not parse the argument! (make"
                            ++ "  sure you enclose the expression in parantheses)"
                else case parse str of
                    Left err  -> Left $ "Parsing Error: " ++ err
                    Right exp -> Right exp
