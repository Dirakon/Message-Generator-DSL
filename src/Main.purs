module Main where

import Prelude

import ActorInstantiation (instantiateAllActors)
import Assertions (applyAssertions)
import Data.Array (concatMap)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (empty, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..))
import Debug (trace)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Evaluate (evaluateMain, tryEvaluateExpression, tryEvaluateExpressionForAllVariables)
import Format (format)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Parse (extractOneStatement, parse, tokensToStatement)
import Tokenization (tokenize)
import Types (Assertion(..), AssertionType(..), DeterministicEvaluatedExpression(..), Expression(..), ExpressionType(..), evaluatedExpressionToArray, nonDeterministicVariableDeclaration)
import Utils (unsafeJust)
import VariableInstantiation (instantiateVariables)

main :: Effect Unit
main =
  launchAff_
    $ do
        -- text <- readTextFile UTF8 "bot.example.txt"
        -- let
        --   evaluatedCode = (format >>> parse >>> evaluate) text
        -- let
        --   textToOutput = case evaluatedCode of
        --     V (Left errors) -> (joinWith "\n" errors)
        --     V (Right res) -> res'
        --       where
        --       res' = case step res of
        --         Nil -> "No message can be generated!"
        --         Cons mes _ -> mes
        let process = (format >>> parse >>> evaluateMain)
        code <- readTextFile UTF8 "in.txt"
        writeTextFile UTF8 "out.txt" (show $ process code)  --((map \{hello}->hello)>>>show) $ hellos
