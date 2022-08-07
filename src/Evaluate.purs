module Evaluate where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (all, and, or)
import Data.Lazy (Lazy)
import Data.List.Lazy (List, length, nil)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Data.Validation.Semigroup (V, invalid)
import Prim.Boolean (False, True)
import Types (Assertion(..), AssertionType(..), BotState, Expression, MacroName, VariableName, Errors)

evaluate :: BotState -> V Errors (List String)
-- TODO: hugely optimize by choosing just one main branch
evaluate state = executeMacro state "Main"

executeMacro :: BotState -> MacroName -> V Errors (List String)
executeMacro state macroName = case lookup macroName state.macros of
  Nothing -> invalid [ "Called macro cannot be found: " <> macroName ]
  Just expression -> do
-- TODO
    let
      { newState, evaluatedExpression } = forceEvaluateExpression state expression
    pure nil

-- | Force evaluatation: if variables are not defined, define them, and thus change state
forceEvaluateExpression :: BotState -> Expression -> { newState :: BotState, evaluatedExpression :: List String }
forceEvaluateExpression state expression = { newState, evaluatedExpression }
-- TODO
  where
  newState = state
  evaluatedExpression = nil

-- | Try evaluatation: if variables are not defined, return Nothing
tryEvaluateExpression :: BotState -> Expression -> Maybe (List String)
tryEvaluateExpression state expression = Just evaluatedExpression
-- TODO
  where
  evaluatedExpression = nil

-- | Check every assertion with 'assertionHolds', accumulate erros if there are any.
assertionsHold :: BotState -> V Errors Boolean
assertionsHold state = ado
  assertionsHold' <- sequence $ map (\assertion -> assertionHolds assertion state) state.assertions
  in all identity assertionsHold'

-- | Return true if assertion is true or if it cannot be checked yet.
-- Throws error if assertion is invalid (non-deterministic)
assertionHolds :: Assertion -> BotState -> V Errors Boolean
assertionHolds (Assertion assertionType _ leftSide rightSide) state =
  fromMaybe (pure true)
    $ ado
        evaluatedLeft <- tryEvaluateExpression state leftSide
        evaluatedRight <- tryEvaluateExpression state rightSide
        in if (1 /= length evaluatedLeft) || (length evaluatedRight /= 1) then
          invalid [ "Assertions cannot be non-deterministic" ]
        else case assertionType of
          ExpressionsEqual -> pure $ evaluatedLeft == evaluatedRight
          ExpressionsDifferent -> pure $ evaluatedLeft /= evaluatedRight

assertionDefined :: Assertion -> BotState -> Boolean
assertionDefined (Assertion _ conceredVariables _ _) state = all (\variable -> variableDefined variable state) conceredVariables

variableDefined :: VariableName -> BotState -> Boolean
variableDefined varName state = case lookup varName state.variables of
  Nothing -> false
  Just _ -> true

-- For OneOf: parallelize and combine
