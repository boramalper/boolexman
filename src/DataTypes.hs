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
module DataTypes where

import Control.Monad
import Data.List
import Test.QuickCheck (sized, oneof, elements, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

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

isNOT :: Expr -> Bool
isNOT (Enot _) = True
isNOT _ = False

isAND :: Expr -> Bool
isAND (Eand _) = True
isAND _ = False

isOR :: Expr -> Bool
isOR (Eor _) = True
isOR _ = False

isIMP :: Expr -> Bool
isIMP (Eimp _ _) = True
isIMP _ = False

isTrueFalse :: Expr -> Bool
isTrueFalse Etrue  = True
isTrueFalse Efalse = True
isTrueFalse _      = False

subexprOf :: Expr -> Expr -> Bool
subexprOf a b =
    (a == b) ||
    case b of
        Efalse               -> False
        Etrue                -> False
        (Esym _)             -> False
        (Enot subexpr)       -> a `subexprOf` subexpr
        (Eimp cond cons)     -> a `subexprOf` cond || a `subexprOf` cons
        (Eite cond cons alt) -> a `subexprOf` cond || a `subexprOf` cons || a `subexprOf` alt
        (Eand subexprs)      -> any (a `subexprOf`) subexprs
        (Eor  subexprs)      -> any (a `subexprOf`) subexprs
        (Exor subexprs)      -> any (a `subexprOf`) subexprs
        (Eiff subexprs)      -> any (a `subexprOf`) subexprs

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

data EntailmentResult = EntailmentResult { condITEeliminations    :: [(Expr, Expr)]
                                         , condPostITEelimination :: Expr
                                         , condIFFeliminations    :: [(Expr, Expr)]
                                         , condPostIFFelimination :: Expr
                                         , condXOReliminations    :: [(Expr, Expr)]
                                         , condPostXORelimination :: Expr
                                         , exprITEeliminations    :: [(Expr, Expr)]
                                         , exprPostITEelimination :: Expr
                                         , exprIFFeliminations    :: [(Expr, Expr)]
                                         , exprPostIFFelimination :: Expr
                                         , exprXOReliminations    :: [(Expr, Expr)]
                                         , exprPostXORelimination :: Expr
                                         , entailment             :: Entailment
                                         }

type Clause = [Expr]
type Step   = [Clause]
type Resolvent = Expr
data ClauseStatus = ResolvedBy Resolvent
                  | Striken
                  deriving (Eq, Show)
type ResolutionSteps = [(Resolvent, Step)]
type ClauseStatuses  = [(Clause, ClauseStatus)]
data Resolution = Resolution { initialStep     :: Step
                             , resolutionSteps :: ResolutionSteps
                             , clauseStatuses  :: ClauseStatuses
                             }

isEsym :: Expr -> Bool
isEsym (Esym _) = True
isEsym _        = False

isEnotX :: (Expr -> Bool) -> Expr -> Bool
isEnotX isX (Enot se) = isX se
isEnotX _   _         = False

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

-- DICTIONARY DATA TYPE
(<++>) :: Eq k => Eq v => [(k,v)] -> [(k,v)] -> [(k,v)]
(<++>) as bs
    | all (\b -> b `notElem` as || snd b == as <!> fst b) bs =
        nub $ as ++ bs
    | otherwise =
        error "lists have keys with different values!"

(<!>) :: Eq k => [(k,v)] -> k -> v
(<!>) dict key =
    case [v | (k,v) <- dict, k == key] of
        []  -> error "invalid operation! <!> on dict where key does not exist!"
        [v] -> v
        _   -> error "corrupt dict! multiple keys found."

(<!?>) :: Eq k => [(k,v)] -> k -> Maybe v
(<!?>) dict key =
    case [v | (k,v) <- dict, k == key] of
        []  -> Nothing
        [v] -> Just v
        _   -> error "corrupt dict! multiple keys found."
