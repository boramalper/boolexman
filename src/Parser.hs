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
module Parser (normalize, parse, mustParse, splitTopOn, parseAll, parseCsSymbols) where

import Data.Char (isAlphaNum, isSpace, isUpper)
import Data.List
import Data.List.Split
import Test.QuickCheck

import Expression

type ParsingError = String

{- parses the comma seperated list of symbols.

WARNING:
  It will err on special symbols True and False! This is intentional.
-}
parseCsSymbols :: String -> Either ParsingError [Expr]
parseCsSymbols []  = Right []
parseCsSymbols str =
    case parseAll $ splitOn "," $ removeSpaces $ normalize str of
        Left err -> Left err
        Right expressions -> if   all isSymbol expressions
                             then Right expressions
                             else Left "not a list of symbols!"
    where
        isSymbol :: Expr -> Bool
        isSymbol (Esym _) = True
        isSymbol _        = False

        -- removeSpaces after the comma
        removeSpaces :: String -> String
        removeSpaces [] = []
        removeSpaces [a] = [a]
        removeSpaces (a:b:s) = if a == ',' && b == ' '
                               then removeSpaces $ ',' : s
                               else a : removeSpaces (b : s)

parse :: String -> Either ParsingError Expr
parse s
    | balanced s = recurse [parseITE, parseIFF, parseIMP, parseOR, parseXOR, parseAND, parseNOT, parseSYM]
    | otherwise  = Left "unbalanced parantheses!"
    where
        recurse :: [String -> Either ParsingError Expr] -> Either ParsingError Expr
        recurse [] = Left "could not parse the expression"
        recurse (p:parsers) = case p $ normalize s of
            Right expr -> Right expr
            Left  err  ->
                if null err then
                    recurse parsers
                else
                    Left $ err ++ "\n  • In the expression:\n      " ++ trimPreceding ' ' s

mustParse :: String -> Expr
mustParse s = case parse s of
    Left  err -> error err
    Right ex' -> ex'

{- show Expr is generous with parantheses and therefore this test might be too
merciful on some edge cases relating to operator precedence, but hey, it's still
better than nothing!
-}
prop_parse :: Expr -> Bool
prop_parse expr = case parse $ show expr of
    Left s      -> error s
    Right expr' -> expr == expr'

{- parseAll tries to parse all the strings in the list `ss`, returning either
the firt parsing error encountered, or the list of (parsed) expressions.
-}
parseAll :: [String] -> Either ParsingError [Expr]
parseAll [] = Right []
parseAll (s:ss) = case parseAll ss of
    Left err -> Left err
    Right expressions -> case parse s of
        Left err -> Left err
        Right expr -> Right $ expr : expressions

parseITE :: String -> Either ParsingError Expr
parseITE s =
    if "if " `isPrefixOf` s then
        case filter (balanced . prefix) $ locateAll [" then "] s of
            [] -> Left "then could not be found"
            [(condition, _, rest)] -> case filter (balanced . prefix) $ locateAll [" else "] rest of
                [] ->  Left "else could not be found"
                [(consequent, _, alternative)] ->
                    case parseAll [drop 3 condition, consequent, alternative] of
                        Right exs -> Right $ Eite (exs!!0) (exs!!1) (exs!!2)
                        Left  err -> Left err
                _ -> Left "multiple else"
            _ -> Left "muliple then"
    else
        case filter (balanced . prefix) $ locateAll ["?"] s of
            [] -> Left ""
            [(condition, _, rest)] -> case filter (balanced . prefix) $ locateAll [":"] rest of
                [] -> Left ": could not be found"
                [(consequent, _, alternative)] ->
                    case parseAll [condition, consequent, alternative] of
                        Right exs -> Right $ Eite (exs!!0) (exs!!1) (exs!!2)
                        Left  err -> Left err
                _ -> Left "multiple :"
            _ -> Left "multiple ?"

parseIFF :: String -> Either ParsingError Expr
parseIFF s =
    case splitTopOn [" iff ", "<=>"] s of
        [_] -> Left ""
        ss -> case parseAll ss of
            Left err -> Left err
            Right sx -> Right $ Eiff sx

parseIMP :: String -> Either ParsingError Expr
parseIMP s =
    case splitTopOn [" implies ", "=>"] s of
        [_] -> Left ""
        ss -> case parseAll ss of
            Left err -> Left err
            Right sx -> Right $ foldr1 Eimp sx

parseOR :: String -> Either ParsingError Expr
parseOR s =
    case splitTopOn [" or ", "v"] s of
        [_] -> Left ""
        ss -> case parseAll ss of
            Left err -> Left err
            Right sx -> Right $ Eor sx

parseXOR :: String -> Either ParsingError Expr
parseXOR s =
    case splitTopOn [" xor ", "+"] s of
        [_] -> Left ""
        ss -> case parseAll ss of
            Left err -> Left err
            Right sx -> Right $ Exor sx

parseAND :: String -> Either ParsingError Expr
parseAND s =
    case splitTopOn [" and ", "^"] s of
        [_] -> Left ""
        ss -> case parseAll ss of
            Left err -> Left err
            Right sx -> Right $ Eand sx

parseNOT :: String -> Either ParsingError Expr
parseNOT s
    | "!"    `isPrefixOf` s = case parse $ drop 1 s of
        e@(Left _) -> e
        (Right e)  -> Right $ Enot e
    | "not " `isPrefixOf` s = case parse $ drop 4 s of
        e@(Left _) -> e
        (Right e)  -> Right $ Enot e
    | otherwise  = Left ""

parseSYM :: String -> Either ParsingError Expr
parseSYM [] = Left "missing symbol/expression!"
parseSYM s
    | s == "True"  = Right Etrue
    | s == "False" = Right Efalse
    | otherwise =
        if isUpper $ head s then
            if all isAlphaNum $ tail s then
                Right $ Esym s
            else
                Left "a symbol can consist of only alphanumeric characters!"
        else
            Left "symbols must start with an uppercase letter!"

{- locateAll returns all occurrences of any of the substrings in the string.
EXAMPLE:
  > locateAll ["<=>"] "((A and B) <=> C) <=> (C <=> (B and A))"
  [
    ("((A and B) ", "<=>", " C) <=> (C <=> (B and A))"),
    ("((A and B) <=> C) ", "<=>", " (C <=> (B and A))"),
    ("((A and B) <=> C) <=> (C ", "<=>", " (B and A))")
  ]
-}
locateAll :: [String] -> String -> [(String, String, String)]
locateAll subs s = recurse "" $ locateFirst subs s
    where
        recurse :: String -> Maybe (String, String, String) -> [(String, String, String)]
        recurse _ Nothing = []
        recurse consumed (Just m) =
            let consumed' = consumed ++ prefix m
            in  (consumed', match m, suffix m) : (recurse (consumed' ++ match m) $ locateFirst subs (suffix m))

{- locateFirst returns the first occurence of any of the substrings in the
string.
EXAMPLE:
  > locateFirst ["iff", "<=>"] "A and B <=> C <=> D"
  ("A and B ", "<=>", " C <=> D")

WARNING:
  If a substring is the prefix of another, the substring that comes foremost
  in the list will be considered as a match.
  TODO: sort the substrings by length in descending order so it will match
        the longest substring first!

  Also, not efficient at all, but as our professors once said in unanimity,
  "we are not concerned about efficiency here".
-}
locateFirst :: [String] -> String -> Maybe (String, String, String)
locateFirst = recurse ""
    where
        recurse :: String -> [String] -> String -> Maybe (String, String, String)
        recurse _ _ [] = Nothing
        recurse consumed subs l@(c:left) =
            let hmm = [s | s <- subs, s `isPrefixOf` l]
            in  if   not $ null hmm
                then Just (consumed, head hmm, drop (length (head hmm)) l)
                else recurse (consumed ++ [c]) subs left

prefix :: (String, String, String) -> String
prefix (p, _, _) = p

match :: (String, String, String) -> String
match (_, m, _) = m

suffix :: (String, String, String) -> String
suffix (_, _, s) = s

prop_locateOne :: [String] -> String -> Bool
prop_locateOne subs str = test $ locateFirst subs str
    where
        test :: Maybe (String, String, String) -> Bool
        test Nothing = True
        test (Just result) = prefix result ++ match result ++ suffix result == str

balanced :: String -> Bool
balanced s = recurse 0 s == 0
    where
        {- recurse (indeed, *loop*) over the string and count the number of
        "open" parentheses by increasing `o` by one each time a '(' is
        encountered and decreasing by one each time a ')' is encountered.

        If `o` ever goes below zero, return -1 (or any non-zero value) to
        indicate that the string is unbalanced.
        EXAMPLE:
          > balanced ")("
          False

          Simply counting the number of opening and closing parentheses might
          fail in cases where the number of closing parentheses is greater
          than the number of opening parentheses at some given point in the
          string.
        -}
        recurse :: Int -> String -> Int
        recurse o [] = o
        recurse o (x:xs)
            | o >= 0 =
                recurse (o + if x == '(' then 1 else if x == ')' then -1 else 0)
                        xs
            | otherwise = -1

{- splitTopOn splits a string into a list of strings at separators `seps`,
respecting the parantheses.
EXAMPLE:
  > splitTopOn ["<=>", "iff"] "A <=> (B <=> C) <=> D"
  ["A "," (B <=> C) "," D"]

  as opposed to splitOn:

  > splitOn "<=>" "A <=> (B <=> C) <=> D"
  ["A "," (B "," C) "," D"]
-}
splitTopOn :: [String] -> String -> [String]
splitTopOn seps = recurse
    where
        recurse :: String -> [String]
        recurse s =
            let q = filter (balanced . prefix) $ locateAll seps s
            in case q of
                [] -> [s]
                [(pre, _, suf)] -> [pre, suf]
                ((pre, mat, _):_) -> pre : (recurse $ drop (length pre + length mat) s)

{- "normalize" the string by

   * converting all whitespace characters (e.g. '\t', '\n', ' ', ...) to the
     space character (i.e. ' ')
   * trim all preceding and trailing space characters
   * reduce continuous space characters into a single space character
-}
normalize :: String -> String
normalize = trimParentheses . reduce . trimTrailing ' ' . trimPreceding ' ' . convert
    where
        -- convert all whitespace characters to the space character (' ')
        convert :: String -> String
        convert [] = []
        convert (x:xs) = (if isSpace x then ' ' else x) : convert xs

        -- reduce continuous space characters into a single space character
        reduce :: String -> String
        reduce [] = []
        reduce s@[_] = s
        reduce (a:b:xs) = (if a == b && b == ' ' then [] else [a]) ++ reduce (b:xs)

-- trim trailing characters
trimTrailing :: Char -> String -> String
trimTrailing ch = reverse . trimPreceding ch . reverse

-- trim preceding characters
trimPreceding :: Char -> String -> String
trimPreceding _ [] = []
trimPreceding ch (x:xs) = if x == ch then trimPreceding ch xs else x:xs

{- Given a *balanced* string, trimParentheses trims the very top-level
parantheses that enclose the whole string.
EXAMPLE:
    > trimParentheses "(A and B)"
    "A and B"

    > trimParentheses "((A and B))"
    "A and B"

    > trimParentheses "(A) and (B)"
    "(A) and (B)"
-}
trimParentheses :: String -> String
trimParentheses []    = []
trimParentheses s@[_] = s
trimParentheses s =
    let mid = init $ tail s
    in if   head s == '(' && last s == ')' && balanced mid
       then trimParentheses mid
       else s
