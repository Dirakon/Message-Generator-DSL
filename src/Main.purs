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
import Types (DeterministicEvaluatedExpression(..), Expression(..), ExpressionType(..), evaluatedExpressionToArray)

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
                  [ [ fromFoldable
                        [ (Tuple "city" (LeafExpression "city"))
                        ]
                    , fromFoldable
                        [ (Tuple "city" (LeafExpression "city2"))
                        ] -- TODO: find out why this should be in the same deterministic variable list
                    ]
                  ]
                  (fromFoldable [])
                  ( Expression
                      ( ConsolidationOf
                          (Expression (VariableCall "city") [ "city" ])
                          (Expression (VariableCall "city") [ "city" ])
                      )
                      [ "city" ]
                  )
        writeTextFile UTF8 "out.txt" textToOutput
