module Evaluate where

import Prelude

import Data.Array (zip)
import Data.Either (Either(..))
import Data.Lazy (Lazy)
import Data.List (filter)
import Data.List.Lazy (List, all, length, nil)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple)
import Data.Validation.Semigroup (V, invalid)
import Prim.Boolean (False, True)
import Types 

evaluate :: BotState -> V Errors EvaluatedExpression
-- TODO: hugely optimize by choosing just one main branch
evaluate state = executeMacro state "Main"

executeMacro :: BotState -> MacroName -> V Errors EvaluatedExpression
executeMacro state macroName = case lookup macroName state.macros of
  Nothing -> invalid [ "Called macro cannot be found: " <> macroName ]
  Just expression -> do
    -- TODO
    let
      { newState, evaluatedExpression } = forceEvaluateExpression state expression
    pure []

-- | Force evaluatation: if variables are not defined, define them, and thus change state
forceEvaluateExpression :: BotState -> Expression -> { newState :: BotState, evaluatedExpression :: EvaluatedExpression }
forceEvaluateExpression state expression = { newState, evaluatedExpression }
  -- TODO
  where
  newState = state

  evaluatedExpression = []

-- | Try evaluatation: if variables are not defined, return Nothing
tryEvaluateExpression :: VariableList -> MacroList -> Expression -> Maybe EvaluatedExpression
tryEvaluateExpression vars macros expression = Just evaluatedExpression
  -- TODO
  where
  evaluatedExpression = []

variableDefined :: VariableName -> BotState -> Boolean
variableDefined varName state = case lookup varName state.variables of
  Nothing -> false
  Just _ -> true


-- TODO: For OneOf: parallelize and combine 



unifyVariableDeclarations:: NonDeterministicVariableDeclaration -> NonDeterministicVariableDeclaration -> NonDeterministicVariableDeclaration
unifyVariableDeclarations deterministicDecs1 deterministicDecs2
  = unifiedDecs
    where
      unifiedDecs = do
        values1 <- deterministicDecs1
        values2 <- deterministicDecs2
        pure $ values1 <> values2