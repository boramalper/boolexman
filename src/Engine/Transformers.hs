module Engine.Transformers where

import DataTypes
import Engine.Util (combine)

normalise :: Expr -> Expr
normalise = removeTriviality . removeRedundancy . flatten

{-| Removes *syntactically* redundant (i.e. repeating) subexpressions in AND, OR,
XOR, IFF expressions, such as:

  (A and B and C) or (A and B and C) or (G and H)
  ^~~~~~~~~~~~~~^
   syntactically
     redundant

HOWEVER, it does not remove/eliminate semantically redundant subexpressions such
as:

  (A and B and C) or (A and B)
  ^~~~~~~~~~~~~~^
    semantically
     redundant

TODO: write a function to remove semantically redundant subexpressions (in DNF
and CNF forms).

ALSO:

   ((A ^ B ^ C) v (A ^ B ^ !C) v ...
 = ((A ^ B) v (A ^ B) v ...

TODO: identify all cases of semantic redundancy!
-}
removeRedundancy :: Expr -> Expr
removeRedundancy (Eand subexprs) = case nub $ map removeRedundancy subexprs of
    [x] -> x
    subexprs' -> eAND subexprs'
removeRedundancy (Eor subexprs) = case nub $ map removeRedundancy subexprs of
    [x] -> x
    subexprs' -> Eor subexprs'
removeRedundancy expr = expr

{- Removes trivial.
-}
removeTriviality :: Expr -> Expr
removeTriviality (Eand subexprs) =
    let subexprs' = filter (/= Etrue) $ map removeTriviality subexprs
    in  if   Efalse `elem` subexprs' || any (\se -> Enot se `elem` subexprs') subexprs'
       then Efalse
       else case subexprs' of
           [x] -> x
           _   -> eAND subexprs'
removeTriviality (Eor subexprs) =
    let subexprs' = filter (/= Efalse) $ map removeTriviality subexprs
    in  if   Etrue `elem` subexprs' || any (\se -> Enot se `elem` subexprs') subexprs'
      then Etrue
      else case subexprs' of
          [x] -> x
          _   -> Eor subexprs'
removeTriviality expr = expr

-- flatten nested expressions
-- TODO: can we reduce the redundancy here?
flatten :: Expr -> Expr
flatten (Eand subexprs) = eAND $ concatMap (
    \se -> case se of
        Eand subexprs' -> subexprs'
        _ -> [se]
    ) $ map flatten subexprs
flatten (Eor subexprs) = eOR $ concatMap (
    \se -> case se of
        Eor subexprs' -> subexprs'
        _ -> [se]
    ) $ map flatten subexprs
flatten (Exor subexprs) = eXOR $ concatMap (
    \se -> case se of
        Exor subexprs' -> subexprs'
        _ -> [se]
    ) $ map flatten subexprs
flatten (Eiff subexprs) = eIFF $ concatMap (
    \se -> case se of
        Eiff subexprs' -> subexprs'
        _ -> [se]
    ) $ map flatten subexprs
flatten (Eimp cond cons) = Eimp (flatten cond) (flatten cons)
flatten (Eite cond cons alt) = Eite (flatten cons) (flatten cons) (flatten alt)
flatten (Enot subexpr)  = Enot $ flatten subexpr
flatten expr | isSymbol expr = expr
             | otherwise     = error "programmer error! update flatten for new non-symbols!"

distributeAND :: Expr -> Maybe Expr
distributeAND (Eand subexprs) =
    let orSubexprs    = filter (\se -> case se of Eor _ -> True; _ -> False) subexprs
        nonOrSubexprs = filter (`notElem` orSubexprs) subexprs
    in  if   null orSubexprs
        then Nothing
        else Just $ Eor $ map (\x -> eAND $ nonOrSubexprs ++ x) (combine $ map (\(Eor x) -> x) orSubexprs)
distributeAND _ = Nothing

distributeOR :: Expr -> Maybe Expr
distributeOR (Eor subexprs) =
    let andSubexprs    = filter (\se -> case se of Eand _ -> True; _ -> False) subexprs
        nonAndSubexprs = filter (`notElem` andSubexprs) subexprs
    in  if   null andSubexprs
        then Nothing
        else Just $ eAND $ map (\x -> eOR $ nonAndSubexprs ++ x) (combine $ map (\(Eand x) -> x) andSubexprs)
distributeOR _ = Nothing

{-| eliminateITE eliminates the given if-then-else expression of form (Γ ? Δ : Ω)
by replacing it with ((Γ ^ Δ) v (!Γ ^ Ω)); if the given expression is of another
form, then Nothing.
-}
eliminateITE :: Expr -> Maybe Expr
eliminateITE (Eite cond cons alt) = Just $ Eor [Eand [cond, cons], Eand [Enot cond, alt]]
eliminateITE _ = Nothing

{-| eliminateIFF eliminates the given if-and-only-if expression of form
(Γ1 <=> Γ2 <=> ... <=> Γn) by replacing it with (!(Γ1 + Γ2 + ... + Γn)); if the
given expression is of another form, then Nothing.
-}
eliminateIFF :: Expr -> Maybe Expr
eliminateIFF (Eiff ses) = Just $ Enot $ Exor ses
eliminateIFF _ = Nothing

{-| eliminateIMP eliminates the given implies expressions of form (Γ1 => Γ2) by
replacing it with (!Γ1 v Γ2); if the given expression is of another form, then
Nothing.
-}
eliminateIMP :: Expr -> Maybe Expr
eliminateIMP (Eimp cond cons) = Just $ Eor [Enot cond, cons]
eliminateIMP _ = Nothing

-- replace, deep first!
replace :: (Expr -> Maybe Expr) -> Expr -> Expr
replace func expr =
    let recurse = replace func
        expr'   = (case expr of
            Enot subexpr       -> Enot $ recurse subexpr
            Eimp cond cons     -> Eimp (recurse cond) (recurse cons)
            Eite cond cons alt -> Eite (recurse cond) (recurse cons) (recurse alt)
            Eand subexprs      -> eAND $ map recurse subexprs
            Exor subexprs      -> eXOR $ map recurse subexprs
            Eor  subexprs      -> eOR  $ map recurse subexprs
            Eiff subexprs      -> eIFF $ map recurse subexprs
            _ -> expr
            )
    in  case func expr' of
        Just newExpr -> replace func newExpr
        Nothing      -> expr'

distributeAllAND :: Expr -> Expr
distributeAllAND = replace distributeAND

distributeAllOR :: Expr -> Expr
distributeAllOR = replace distributeOR

eliminateAllITE :: Expr -> Expr
eliminateAllITE = replace eliminateITE

eliminateAllIFF :: Expr -> Expr
eliminateAllIFF = replace eliminateIFF

eliminateAllIMP :: Expr -> Expr
eliminateAllIMP = replace eliminateIMP

-- Depth First Yield
yield :: (Expr -> Maybe Expr) -> Expr -> [(Expr, Expr)]
yield func expr =
    let replace'   = replace func
        recurse    = yield func
        innerYield = (case expr of
            Enot subexpr       -> recurse subexpr
            Eimp cond cons     -> recurse cond ++ recurse cons
            Eite cond cons alt -> recurse cond ++ recurse cons ++ recurse alt
            Eand subexprs      -> concatMap recurse subexprs
            Exor subexprs      -> concatMap recurse subexprs
            Eor  subexprs      -> concatMap recurse subexprs
            Eiff subexprs      -> concatMap recurse subexprs
            _ -> []
            )
        newX = (case expr of
            Enot subexpr       -> Enot $ replace' subexpr
            Eimp cond cons     -> Eimp (replace' cond) (replace' cons)
            Eite cond cons alt -> Eite (replace' cond) (replace' cons) (replace' alt)
            Eand subexprs      -> eAND $ map replace' subexprs
            Exor subexprs      -> eXOR $ map replace' subexprs
            Eor  subexprs      -> eOR  $ map replace' subexprs
            Eiff subexprs      -> eIFF $ map replace' subexprs
            _ -> expr
            )
    in  case func newX of
        Just expr' -> innerYield ++ [(canonical newX, canonical expr')] ++ recurse expr'
        Nothing    -> innerYield

distributionsAND :: Expr -> [(Expr, Expr)]
distributionsAND = yield distributeAND

distributionsOR :: Expr -> [(Expr, Expr)]
distributionsOR = yield distributeOR

eliminationsITE :: Expr -> [(Expr, Expr)]
eliminationsITE = yield eliminateITE

eliminationsIFF :: Expr -> [(Expr, Expr)]
eliminationsIFF = yield eliminateIFF

eliminationsIMP :: Expr -> [(Expr, Expr)]
eliminationsIMP = yield eliminateIMP
