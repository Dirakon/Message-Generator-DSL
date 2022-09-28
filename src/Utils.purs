module Utils where

import Prelude
import Types

import Data.Array as A
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.List (List, elem, (:))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Map.Internal as MI
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, match)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..))
import Partial.Unsafe (unsafePartial)
import Random.LCG (Seed, unSeed)

extractAssertions :: List Statement -> List Assertion
extractAssertions =
  L.mapMaybe
    ( case _ of
        AssertionStatement assertion -> Just assertion
        _ -> Nothing
    )

extractAssigments :: List Statement -> List Assigment
extractAssigments =
  L.mapMaybe
    ( case _ of
        AssigmentStatement assigment -> Just assigment
        _ -> Nothing
    )

extractVariableAssigments :: List Statement -> List (Tuple (Array VariableName) Expression)
extractVariableAssigments statements =
  L.mapMaybe
    ( case _ of
        (AssigmentStatement (Assigment (VariableSignature varNames) expr)) -> Just $ Tuple varNames expr
        _ -> Nothing
    )
    statements

unsafeHead :: forall a. Array a -> a
unsafeHead = unsafePartial (\[a] -> a)

randomElementArray :: forall a. Array a -> Seed -> Maybe a
randomElementArray arr seed = A.index arr i
  where
    i = (unSeed seed) `mod` (A.length arr)

partitionVariableAssigments :: List (Tuple (Array VariableName) Expression) -> { singleVariableAssigments :: List (Tuple VariableName Expression), multiVariableAssigments :: List (Tuple (Array VariableName) Expression) }
partitionVariableAssigments assigments =
  { singleVariableAssigments: map (\(Tuple vars expr) -> Tuple (unsafeHead vars) expr) singleVariableAssigments
  , multiVariableAssigments
  }
  where
  isSingleVar (Tuple [ _ ] _) = true

  isSingleVar _ = false

  { yes: singleVariableAssigments, no: multiVariableAssigments } = L.partition isSingleVar assigments

extractMacros :: List Statement -> MacroList
extractMacros statements =
  M.fromFoldable
    $ L.mapMaybe
        ( case _ of
            (AssigmentStatement (Assigment (MacroSignature macroName) expr)) -> Just $ Tuple macroName expr
            _ -> Nothing
        )
        statements

presentInAssertion :: VariableName -> Assertion -> Boolean
presentInAssertion varName (Assertion _ (Expression _ actors1) (Expression _ actors2)) = varName `elem` (actors1 <> actors2)

match' :: Regex -> String -> Array (Maybe String)
match' regex string = case match regex string of
  Nothing -> []
  Just arr -> toArray arr

unsafeJust :: forall a. Maybe a -> a
unsafeJust =
  unsafePartial
    $ case _ of
        Just a -> a

fromError :: forall a. V (Array String) a -> Maybe a
fromError (V (Right a)) = Just a
fromError _ = Nothing