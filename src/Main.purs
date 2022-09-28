module Main where

import Prelude

import ActorInstantiation (instantiateAllActors)
import Assertions (applyAssertions)
import Data.Array as A
import Data.Either (Either(..))
import Data.List (List(..), foldl, (:))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..))
import Debug (trace)
import Determination (determine)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log, logShow)
import Evaluate (evaluateMain, tryEvaluateExpression, tryEvaluateExpressionForAllVariables)
import Format (format)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Parse (extractOneStatement, parse, tokensToStatement)
import Random.LCG (lcgNext, randomSeed)
import Tokenization (tokenize)
import Types (Assertion(..), AssertionType(..), CommonMetaData, DeterministicEvaluatedExpression(..), Expression(..), ExpressionType(..), MacroName, NonDeterministicVariableList, evaluatedExpressionToArray, nonDeterministicVariableDeclaration)
import Utils (randomElementArray, unsafeJust)
import VariableInstantiation as VariableInstantiation

generateOneMessage ∷ CommonMetaData -> Effect String
generateOneMessage { evaluatedVariables, lazyVariables, macros } = do
  seed <- randomSeed
  pure $ toRightFormat 
    $ do
        main <- findMain macros
        determinedExpression seed main
  where
  findMain macros = case M.lookup "Main" macros of
    Nothing -> Nothing
    Just macroExpression -> Just macroExpression

  determinationState seed = { determinedLazyVariables: M.empty, undeterminedLazyVariables: lazyVariables, seed }

  determinedExpression seed main = determine (determinationState seed') macros (pickedVariableSet) main
    where
    initialState = Tuple seed M.empty

    foldFunction (Tuple seed curMap) mapCollection = Tuple (lcgNext seed) (M.unionWith (\a _ -> a) curMap (newMap mapCollection seed))

    newMap mapCollection seed = fromMaybe M.empty $ randomElementArray mapCollection seed

    Tuple seed' pickedVariableSet = foldl (foldFunction) initialState evaluatedVariables


  toRightFormat (Just (Tuple _ (LeafExpression answer))) = answer
  toRightFormat (Just (Tuple a (TreeExpression [maybeAnswer]))) = toRightFormat (Just (Tuple a maybeAnswer))

  toRightFormat _ = "ERROR GENERATING MESSAGE"

initializeVariables :: String -> CommonMetaData
initializeVariables = (format >>> parse)
