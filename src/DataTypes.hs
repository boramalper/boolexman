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
module DataTypes where

import Control.Monad
import Data.List
import Data.List.Split
import Debug.Trace
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

data Expr = Efalse
          | Etrue
          | Esym String
          | Enot Expr
          | Eimp Expr Expr
          | Eite Expr Expr Expr
          | Eand [Expr]
          | Eor  [Expr]
          | Exor [Expr]
          | Eiff [Expr]
    deriving Eq

instance Ord Expr where
    Efalse   <= _        = True
    Etrue    <= Efalse   = False
    Etrue    <= _        = True
    (Esym _) <= Efalse   = False
    (Esym _) <= Etrue    = False
    (Esym a) <= (Esym b) = a <= b
    (Esym _) <= _        = True
    (Enot _) <= Efalse   = False
    (Enot _) <= Etrue    = False
    (Enot _) <= (Esym _) = False
    (Enot a) <= (Enot b) = a <= b
    a        <= b        = cardinality a <= cardinality b
        where
            -- cardinality is the total number of subexpressions
            cardinality :: Expr -> Int
            cardinality Efalse           = 1
            cardinality Etrue            = 1
            cardinality (Esym _)         = 1
            cardinality (Enot subexpr)   = 1 + cardinality subexpr
            cardinality (Eimp cond cons) = 1 + cardinality cond + cardinality cons
            cardinality (Eite cond cons alt) = 1 + cardinality cond + cardinality cons + cardinality alt
            cardinality (Eand subexprs) = 1 + sum (map cardinality subexprs)
            cardinality (Eor  subexprs) = 1 + sum (map cardinality subexprs)
            cardinality (Eiff subexprs) = 1 + sum (map cardinality subexprs)
            cardinality (Exor subexprs) = 1 + sum (map cardinality subexprs)

isNOT (Enot _) = True
isNOT _ = False

isAND (Eand _) = True
isAND _ = False

isOR (Eor _) = True
isOR _ = False

isIMP (Eimp _ _) = True
isIMP _ = False

isTrueFalse Etrue  = True
isTrueFalse Efalse = True
isTrueFalse _      = False

-----------------------------------------
-- SEQUENT CALCULUS

--                  TURNSTILE
--                     HERE
data Line = Line [Expr]       [Expr] deriving (Eq)

instance Show Line where
    show (Line conds exprs) = slist conds ++ " |- " ++ slist exprs
        where
            slist :: [Expr] -> String
            slist [] = ""
            slist l = foldr1 (\a b -> a ++ ", " ++ b) $ map show l

data Entailment = I Line
                | F Line  -- indicates failure
                | Land Line Entailment
                | Ror  Line Entailment
                | Rimp Line Entailment
                | Lnot Line Entailment
                | Rnot Line Entailment
                | Limp Line Entailment Entailment
                | Lor  Line [Entailment]
                | Rand Line [Entailment]
                deriving (Eq)

-- mp4man style, bottom up.
instance Show Entailment where
    show (I line)               = showEntailment "I"   line []
    show (F line)               = showEntailment "F"   line []
    show (Land line entailment) = showEntailment "^L"  line [entailment]
    show (Ror line entailment)  = showEntailment "vR"  line [entailment]
    show (Rimp line entailment) = showEntailment "=>R" line [entailment]
    show (Lnot line entailment) = showEntailment "!L"  line [entailment]
    show (Rnot line entailment) = showEntailment "!R"  line [entailment]
    show (Limp line ent1 ent2)  = showEntailment "=>L" line [ent1, ent2]
    show (Lor   line ents)      = showEntailment "vL"  line ents
    show (Rand  line ents)      = showEntailment "^R"  line ents

showEntailment :: String -> Line -> [Entailment] -> String
showEntailment rule line entailments =
   let line'        = show line
       entailments' = boxAppend "   " $ map show entailments
   in     entailments'
       ++ "\n" ++ replicate (max (width entailments') (length line')) '─' ++ " (" ++ rule ++ ")"
       ++ "\n" ++ center (max (width entailments') (length line')) line'

center :: Int -> String -> String
center width s
    | length s <= width = replicate ((width - length s) `div` 2) ' ' ++ s
    | otherwise = error "string is longer than the width"

{-
TODO: refactor to increase readability.
this is basically a primitive layout engine, oh god.

EXAMPLE:
  > putStrLn $ boxAppend " | " ["AAA\nBBBBBB\nCCCCCC", "ZZZZZ\nQQ"]
  AAA    |
  BBBBBB | ZZZZZ
  CCCCCC | QQ
-}
boxAppend :: String -> [String] -> String
boxAppend _ [] = ""
boxAppend padding strings =
    let
        listOfLines = map (splitOn "\n") strings :: [[String]]
        listOfFixedWidthLines = map (\lines' -> map (fixedWidth (longest lines')) lines') listOfLines :: [[String]]
        listOfSameHeightFixedWidthLines = map (fixedHeight (longest listOfFixedWidthLines)) listOfFixedWidthLines :: [[String]]
    in
        foldr1 (\a b -> a ++ "\n" ++ b) $ map (\i -> append $ map (\x -> x !! i) listOfSameHeightFixedWidthLines) [0..length (head listOfSameHeightFixedWidthLines) - 1]
    where
        append :: [String] -> String
        append = foldr1 (\a b -> a ++ padding ++ b)

        longest :: [[a]] -> Int
        longest = maximum . map length

        fixedHeight :: Int -> [String] -> [String]
        fixedHeight height strs
            | length strs <= height = replicate (height - length strs) (replicate (length $ head strs) ' ') ++ strs
            | otherwise = error "strs is taller than height!"

        fixedWidth :: Int -> String -> String
        fixedWidth width str
            | length str <= width = str ++ replicate (width - length str) ' '
            | otherwise = error "str is longer than width!"

width :: String -> Int
width = maximum . map length . splitOn "\n"

data EntailmentResult = EntailmentResult { condITEeliminations    :: [(Expr, Expr)]
                                         , condIFFeliminations    :: [(Expr, Expr)]
                                         , condXOReliminations    :: [(Expr, Expr)]
                                         , exprITEeliminations    :: [(Expr, Expr)]
                                         , exprIFFeliminations    :: [(Expr, Expr)]
                                         , exprXOReliminations    :: [(Expr, Expr)]
                                         , condPostXORelimination :: Expr
                                         , exprPostITEelimination :: Expr
                                         , entailment             :: Entailment
                                         }

instance Show EntailmentResult where
    show res = show $ entailment res

-----------------------------------------
type Clause = [Expr]
type Step   = [Clause]
type Resolvent = Expr
data ClauseStatus = ResolvedBy Resolvent
                  | Striken
                  deriving Show
type ResolutionSteps = [(Resolvent, Step)]
type ClauseStatuses  = [(Clause, ClauseStatus)]
-- TODO: can we make this better? the data types I mean for linked & related list
data Resolution = Resolution { initialStep     :: Step
                             , resolutionSteps :: ResolutionSteps
                             , clauseStatuses  :: ClauseStatuses
                             }
-----------------------------------------
isSymbol :: Expr -> Bool
isSymbol (Esym _) = True
isSymbol Etrue    = True
isSymbol Efalse   = True
isSymbol _ = False

isNegSymbol :: Expr -> Bool
isNegSymbol (Enot subexpr) = isSymbol subexpr
isNegSymbol _ = False

instance Show Expr where
    show (Enot se) = '!' : show se
    show (Eimp cond cons) = parens $ show cond ++ " => " ++ show cons
    show (Eite cond cons alt) = parens $ show cond ++ " ? " ++ show cons ++ " : " ++ show alt
    show (Eand es) = parens $ "" ++ (foldr1 (\a b -> a ++  " ^ "  ++ b) $ map show es)
    show (Eor es)  = parens $ "" ++ (foldr1 (\a b -> a ++  " v "  ++ b) $ map show es)
    show (Exor es) = parens $ "" ++ (foldr1 (\a b -> a ++  " + "  ++ b) $ map show es)
    show (Eiff es) = parens $ "" ++ (foldr1 (\a b -> a ++ " <=> " ++ b) $ map show es)
    show (Esym sym) = sym
    show Etrue = "True"
    show Efalse = "False"

-- helper functions
eAND :: [Expr] -> Expr
eAND []  = Etrue
eAND [x] = x
eAND subexprs = Eand subexprs

eOR :: [Expr] -> Expr
eOR []  = Efalse
eOR [x] = x
eOR subexprs = Eor subexprs

eXOR :: [Expr] -> Expr
eXOR subexprs
    | length subexprs >= 2 = Exor subexprs
    | otherwise = error "eXOR must have at least two subexpressions!"

eIFF :: [Expr] -> Expr
eIFF subexprs
    | length subexprs >= 2 = Eiff subexprs
    | otherwise = error "eIFF must have at least two subexpressions!"

-- TODO: I don't think this is a "high quality" one...
instance Arbitrary Expr where
    arbitrary = sized prop
        where
            prop n | n <= 0     = atom
                   | otherwise  = oneof [ atom
                                        , liftM  Enot subform
                                        , liftM2 Eimp subform subform
                                        , liftM3 Eite subform' subform' subform'
                                        , liftM  Eand $ vectorOf 4 subform
                                        , liftM  Eor  $ vectorOf 4 subform
                                        , liftM  Exor $ vectorOf 4 subform
                                        , liftM  Eiff $ vectorOf 4 subform
                                        ]
                    where
                        atom = oneof [liftM Esym (elements ["P", "Q", "R", "S", "T"]),
                                        elements [Etrue, Efalse]]
                        subform  = prop (n `div` 8)
                        subform' = prop (n `div` 16)

-- SET: Sub-Expression Tree
data SET = SET Expr [SET] deriving Eq
data SubexpressionsResult = SubexpressionsResult { set  :: SET
                                                 , list :: [Expr]
                                                 }

data EvalResult = EvalResult { redundantTrueSymbols  :: [Expr]
                             , redundantFalseSymbols :: [Expr]
                             , productOfSums         :: [[Expr]]
                             , trueEliminations      :: [(Expr, [Expr])]
                             , postTrueElimination   :: [[Expr]]
                             , sumOfProducts         :: [[Expr]]
                             , falseEliminations     :: [(Expr, [Expr])]
                             , postFalseElimination  :: [[Expr]]
                             , result                :: Expr
                             } deriving (Show)

parens :: String -> String
parens s = '(' : s ++ ")"
