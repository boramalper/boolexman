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
{- DISCLAIMER: I didn't study Monads and IO enough, nor never really understood
them, and it shows:
-}

import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.List.Split (splitOn)
import System.Console.Haskeline
import Text.Regex.TDFA

import DataTypes
import Engine.Commands
import Parser
import View

main :: IO ()
main = do
    putStrLn "boolexman - boolean expression manipulator | v0.1.0.0"
    runInputT defaultSettings (loop 1)

loop :: Integer -> InputT IO ()
loop no = do
    line <- getInputLine $ formatNo 4 no ++ "> "
    case line of
        Nothing -> outputStrLn "\nEOF received, quitting..."
        Just "" -> loop $ no + 1
        Just line -> let line' = normaliseString line
                     in  unless (map toLower line' == "quit") $ do
                         case process line' of
                             Left  err -> printError err
                             {- I don't know what liftIO does but it works!
                             https://stackoverflow.com/questions/28053526/perform-simple-io-in-haskeline-inside-inputt-monad-without-having-to-resort-to
                             -}
                             Right out -> liftIO $ viewLess out
                         loop $ no + 1
    where
        unless :: Bool -> InputT IO() -> InputT IO()
        -- https://hackage.haskell.org/package/base-4.10.1.0/docs/src/Control.Monad.html#unless
        unless p s =  if p then pure () else s

        formatNo :: Int -> Integer -> String
        formatNo minLength no =
            let noStr = show no
            in replicate (minLength - length noStr) ' ' ++ noStr

process :: String -> Either String String
process line =
    let command  = map toLower $ head $ splitOn " " line
        argument = drop (length command + 1) line  -- +1 for the space character
    in case command of
        "tabulate" -> case parseSoleExpression argument of
            Left err   -> Left err
            Right expr -> Right $ viewTabulate expr $ tabulate expr
        "subexpressions" -> case parseSoleExpression argument of
            Left  err  -> Left err
            Right expr -> Right $ viewSubexpressions expr $ subexpressions expr
        "symbols" -> case parseSoleExpression argument of
            Left  err -> Left err
            Right expr -> Right $ viewSymbols expr $ symbols expr
        -- TODO: This looks really ugly, isn't there a neater way?
        "eval" -> -- eval [P, Q] [R, S, T] ((P and Q and R) or (S implies T))
            let (_, _, _, matches) = argument =~ (symbolListCRE ++ ' ' : symbolListCRE ++ ' ' : expressionCRE) :: (String, String, String, [String])
            in  if   length matches /= 3
                then Left "Parsing Error: supply two lists of symbols, and an expression!"
                else case parseCsSymbols $ matches !! 0 of
                    Left err          -> Left $ "Parsing Error: " ++ err ++ "(in the first list)"
                    Right trueSymbols -> case parseCsSymbols $ matches !! 1 of
                        Left err           -> Left $ "Parsing Error: " ++ err ++ "(in the second list)"
                        Right falseSymbols -> case parse $ matches !! 2 of
                            Left err  -> Left $ "Parsing Error: " ++ err ++ "(in the expression)"
                            Right expr -> Right $ viewEval trueSymbols falseSymbols expr $ eval trueSymbols falseSymbols expr -- "tS: " ++ show trueSymbols ++ "  fS: " ++ show falseSymbols ++ "  ex: " ++ show exp
        "todnf" -> case parseSoleExpression argument of
            Left  err -> Left err
            Right expr -> Right $ viewDNF expr $ toDNF expr
        "tocnf" -> case parseSoleExpression argument of
            Left  err -> Left err
            Right expr -> Right $ viewCNF expr $ toCNF expr
        "resolve" -> case parseSoleExpression argument of
            Left  err -> Left err
            Right expr -> Right $ viewResolution expr $ resolve expr
        "entail" ->
            let (_, _, _, expressions) = argument =~ (expressionCRE ++ " " ++ expressionCRE)
                                         :: (String, String, String, [String])
            in  if   length expressions /= 2
                then    Left $ "Parsing Error: could not parse the argument! (make"
                     ++ "  sure you enclose the expressions in parantheses)"
                else case parseAll expressions of
                    Left err    -> Left $ "Parsing Error: " ++ err
                    Right [cond, expr] ->
                        if   all (not . (`subexprOf` cond)) [Etrue, Efalse] && all (not . (`subexprOf` expr)) [Etrue, Efalse]
                        then Right $ viewEntailment cond expr $ entail cond expr
                        else Left $ "Semantic Error: True and/or False cannot appear in neither"
                             ++ " condition nor expression of an entailment!"
        command ->
            Left $ "Error: Unknown command: " ++ command ++ "  "
    where
        -- CAPTURES the expression enclosed in parantheses
        expressionCRE = "\\((.*)\\)"
        symbolRE     = "[A-Z][a-zA-Z0-9]*"
        -- CAPTURES the comma-separated list of symbols between the square
        -- parantheses
        -- symbolListCRE = "\\[(" ++ symbolRE ++ "(?: ?, ?" ++ symbolRE ++ ")*)\\]"
        symbolListCRE = "\\[(.*)\\]"

        parseSoleExpression :: String -> Either String Expr
        parseSoleExpression str =
            let (a, expression, b) = str =~ expressionCRE :: (String, String, String)
            in  if   null expression || not (null a) || not (null b)
                then Left $    "Parsing Error: could not parse the argument! (make"
                            ++ "  sure you enclose the expression in parantheses)"
                else case parse str of
                    Left err  -> Left $ "Parsing Error: " ++ err
                    Right exp -> Right exp
