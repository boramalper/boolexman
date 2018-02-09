module Resolution where

import Data.List
import Expression

type Clause = [Expr]
type Step   = [Clause]
type Resolvent = Expr
data ClauseStatus = ResolvedBy Resolvent
                  | Striken
                  deriving Show
type ResolutionSteps = [(Resolvent, Step)]
type ClauseStatuses  = [(Clause, ClauseStatus)]
data Resolution = Resolution { initialStep     :: Step
                             , resolutionSteps :: ResolutionSteps
                             , clauseStatuses  :: ClauseStatuses
                             }

resolve :: Expr -> Resolution
resolve expr = undefined
    where
        recurse :: [Clause] -> (ResolutionSteps, ClauseStatuses)
        recurse clauses = let resolvent         = findSuitableResolvent clauses
                              usedClauses       = filter (\clause -> resolvent `elem` clause || Enot resolvent `elem` clause) clauses
                              newClauses        = calcNewClauses resolvent clauses
                              strikenClauses    = filter shouldStrike newClauses
                              dict              = map (\c -> (c, ResolvedBy resolvent)) usedClauses ++ map (\c -> (c, Striken)) strikenClauses
                              (nextRL, nextCD) = recurse $ (clauses \\ usedClauses) ++ (newClauses \\ strikenClauses)
                          in  ((resolvent, newClauses) : nextRL, dict ++ nextCD)

        calcNewClauses :: Resolvent -> [Clause] -> [Clause]
        calcNewClauses resolvent clauses = let positiveClauses = filter (\c ->      resolvent `elem` c) clauses
                                               negativeClauses = filter (\c -> Enot resolvent `elem` c) clauses
                                           in  map (uncurry $ merge resolvent) $ cartesianProduct positiveClauses negativeClauses

        merge :: Resolvent -> Clause -> Clause -> Clause
        merge resolvent positive negative = (resolvent `delete` positive) ++ (Enot resolvent `delete` negative)

        findSuitableResolvent :: [Clause] -> Expr
        findSuitableResolvent clauses = let symbols = nub $ concat clauses
                                        in  head $ filter (\sym -> any (sym `elem`) clauses && any (Enot sym `elem`) clauses) symbols

        shouldStrike :: Clause -> Bool
        shouldStrike exprs = any (\expr -> Enot expr `elem` exprs) exprs

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct as bs = concatMap (\a -> map (\b -> (a, b)) bs) as
