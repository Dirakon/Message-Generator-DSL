module Determination where

import Prelude
import Types

import Data.Array as A
import Data.Foldable (foldM, foldl)
import Data.List (List, (:))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Evaluate (consolidateEvaluatedExpressions)
import Random.LCG (Seed, lcgNext)
import Utils (fromError, randomElementArray, unsafeJust)

type DeterminationState
  = { determinedLazyVariables :: Map VariableName DeterministicEvaluatedExpression, undeterminedLazyVariables :: Map VariableName Expression, seed :: Seed }

determine :: DeterminationState -> MacroList -> DeterministicVariableDeclaration -> Expression -> Maybe (Tuple DeterminationState DeterministicEvaluatedExpression)
determine oldState@{ determinedLazyVariables, undeterminedLazyVariables, seed } macros evaluatedVars (Expression exprType _) = case exprType of
  (OneOf candidates) -> recSingle (advanceSeed oldState) chosenCandidate
    -- TODO: Apply deterministic operation on evaluated expressions
    where
    chosenCandidate = unsafeJust $ randomElementArray candidates seed -- Here we assume that OneOf has at least one candidate (which it should)
  (TupleOf expressions) ->
    wrapWithoutAction
      $ recMultiple oldState (L.fromFoldable expressions)
  (Literal literal) -> Just (Tuple oldState (LeafExpression literal))
  (ConsolidationOf expr1 expr2) ->
    wrapWithOperation
      (\a b -> fromError $ consolidateEvaluatedExpressions a b)
      (recMultiple oldState (expr1 : expr2 : L.Nil))
  (MacroCall macroName) -> do
    macro <- M.lookup macroName macros
    ans <- recSingle oldState macro
    pure ans
  (VariableCall var) -> case ((M.lookup var determinedLazyVariables) /\ (M.lookup var evaluatedVars) /\ (M.lookup var undeterminedLazyVariables)) of
    Just evaluatedVar /\ _ /\ _ -> Just (Tuple oldState evaluatedVar)
    _ /\ Just evaluatedVar /\ _ -> Just (Tuple oldState evaluatedVar)
    _ /\ _ /\ Just varConstructor -> map (addNewVariable var) (recSingle oldState varConstructor)
    _ -> Nothing
    where
    addNewVariable var (Tuple { determinedLazyVariables, undeterminedLazyVariables, seed } varValue) =
      Tuple
        { determinedLazyVariables:  M.insert var varValue determinedLazyVariables
        , undeterminedLazyVariables: M.delete var undeterminedLazyVariables
        , seed
        }
        varValue
  where
  advanceSeed { determinedLazyVariables, undeterminedLazyVariables, seed } = { determinedLazyVariables, undeterminedLazyVariables, seed: lcgNext seed }

  recSingle :: DeterminationState -> Expression -> Maybe (Tuple DeterminationState DeterministicEvaluatedExpression)
  recSingle oldState (expr) = determine oldState macros evaluatedVars expr

  recMultiple :: DeterminationState -> List Expression -> Maybe (Tuple DeterminationState EvaluatedExpressionContainer)
  recMultiple oldState (expr : xs) = do
    Tuple newState evaledExpression <- recSingle oldState expr
    Tuple newState' otherEvaledExpressions <- recMultiple newState xs
    pure $ Tuple newState' ([ evaledExpression ] <> otherEvaledExpressions)

  recMultiple state (L.Nil) = Just (Tuple state [])

  wrapWithoutAction :: Maybe (Tuple DeterminationState EvaluatedExpressionContainer) -> Maybe (Tuple DeterminationState DeterministicEvaluatedExpression)
  wrapWithoutAction = map (\(Tuple a container) -> Tuple a (TreeExpression container))

  wrapWithOperation ::
    (DeterministicEvaluatedExpression -> DeterministicEvaluatedExpression -> Maybe DeterministicEvaluatedExpression) ->
    Maybe (Tuple DeterminationState EvaluatedExpressionContainer) -> Maybe (Tuple DeterminationState DeterministicEvaluatedExpression)
  wrapWithOperation operation maybeAnswer = do
    Tuple state exprs <- maybeAnswer
    { head, tail } <- A.uncons exprs
    unifiedExprs <- foldM operation head tail
    pure $ Tuple state unifiedExprs 
