module Main where

import Prelude

import Assertions (applyAssertions)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (empty, fromFoldable)
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Evaluate (tryEvaluateExpression, tryEvaluateExpressionForAllVariables)
import Format (format)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Parse (extractOneStatement, parse, tokensToStatement)
import Tokenization (tokenize)
import Types (Assertion(..), AssertionType(..), DeterministicEvaluatedExpression(..), Expression(..), ExpressionType(..), evaluatedExpressionToArray, nonDeterministicVariableDeclaration)

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
        let
          variables =
            [ nonDeterministicVariableDeclaration
                [ "person", "pronoun" ]
                [ TreeExpression [ LeafExpression "woman", LeafExpression "she" ]
                , TreeExpression [ LeafExpression "man", LeafExpression "he" ]
                ]
            , nonDeterministicVariableDeclaration
                [ "person2", "pronoun2" ]
                [ TreeExpression [ LeafExpression "woman", LeafExpression "she" ]
                , TreeExpression [ LeafExpression "man", LeafExpression "he" ]
                ]
            , nonDeterministicVariableDeclaration
                [ "city2" ]
                [ LeafExpression "piter", LeafExpression "sverdlovsk" ]
            ]
        let
          macros = empty
        let
          assertedVariables = applyAssertions
            [
              Assertion ExpressionsDifferent
                (Expression (VariableCall "pronoun") ["pronoun"])
                (Expression (VariableCall "pronoun2") ["pronoun2"])
            ]
            macros
            variables
        let
          textToOutput =
            show
              $ tryEvaluateExpressionForAllVariables
                  assertedVariables
                  macros
                  ( Expression
                      ( ConsolidationOf
                          ( Expression
                              ( ConsolidationOf
                                  (Expression (VariableCall "person") [ "person" ])
                                  (Expression (VariableCall "pronoun") [ "pronoun" ])
                              )
                              [ "person", "pronoun" ]
                          )
                          ( Expression
                              ( ConsolidationOf
                                  (Expression (VariableCall "person2") [ "person2" ])
                                  (Expression (VariableCall "pronoun2") [ "pronoun2" ])
                              )
                              [ "person2", "pronoun2" ]
                          )
                      )
                      [ "person", "pronoun", "person2", "pronoun2" , "city2"]
                  )
        let funcChain = tokenize <<< L.fromFoldable <<< toCharArray 
        writeTextFile UTF8 "out.txt" (show $ extractOneStatement ("[$macro,$var,$really]=[$a,[#c]]":Nil))
