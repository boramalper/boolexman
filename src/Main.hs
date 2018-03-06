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
module Main where
{- DISCLAIMER: I didn't study Monads and IO enough, nor never really understood
them, and it shows:
-}

import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import System.Console.Haskeline

import Parser (normaliseString)
import Process
import View (printError, viewLess)

main :: IO ()
main = do
    putStrLn "boolexman - boolean expression manipulator | v0.2.0.0"
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
