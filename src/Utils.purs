module Utils where

import Prelude
import Types

import Data.Array as A
import Data.List (List, (:))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Map.Internal as MI
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)


extractAssertions :: List Statement -> List Assertion
extractAssertions = L.mapMaybe
      ( case _ of
          AssertionStatement assertion -> Just assertion
          _ -> Nothing
      )
      

extractAssigments :: List Statement -> List Assigment
extractAssigments = L.mapMaybe
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

extractMacros :: List Statement -> MacroList
extractMacros statements = 
    M.fromFoldable
      $ L.mapMaybe
          ( case _ of
              (AssigmentStatement (Assigment (MacroSignature macroName) expr)) -> Just $ Tuple macroName expr
              _ -> Nothing
          )
          statements




unsafeJust :: forall a. Maybe a -> a
unsafeJust =
  unsafePartial
    $ case _ of
        Just a -> a