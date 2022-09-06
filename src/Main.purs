module Main where

import Prelude
import Data.Either (Either(..))
import Data.List.Lazy (Step(..), step)
import Data.Map (fromFoldable)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Evaluate (evaluate, tryEvaluateExpression, tryEvaluateExpressionForAllVariables)
import Format (format)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Parse (parse)
import Types (DeterministicEvaluatedExpression(..), Expression(..), ExpressionType(..), evaluatedExpressionToArray, nonDeterministicVariableDeclaration)

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
          textToOutput =
            show
              $ tryEvaluateExpressionForAllVariables
                  [ nonDeterministicVariableDeclaration
                      [ "person", "pronoun" ]
                      [ TreeExpression [ LeafExpression "woman", LeafExpression "she" ]
                      , TreeExpression [ LeafExpression "man", LeafExpression "he" ]
                      ]
                  , nonDeterministicVariableDeclaration
                      [ "city2" ]
                      [ LeafExpression "piter", LeafExpression "sverdlovsk" ]
                  ]
                  (fromFoldable [])
                  ( Expression
                      ( ConsolidationOf
                          ( Expression
                              (ConsolidationOf 
                                (Expression (VariableCall "person") [ "person" ]) 
                                (Expression (VariableCall "pronoun") [ "pronoun" ]) 
                                )
                                [ "person", "pronoun" ]
                          )
                          (Expression (VariableCall "city2") [ "city2" ])
                      )
                      [ "person", "pronoun", "city2" ]
                  )
        writeTextFile UTF8 "out.txt" textToOutput
