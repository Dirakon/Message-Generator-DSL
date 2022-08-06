module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff, launchAff_, makeAff)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)

main :: Effect Unit
main =
  launchAff_
    $ do
        text <- readTextFile UTF8 "bot.example.txt"
        writeTextFile UTF8 "out.txt" text

type MacroName
  = String

type VariableName
  = String

data Expression
  = OneOf Expression Expression
  | ConsolidationOf Expression Expression
  | MacroCall MacroName
  | VariableCall VariableName
  | Literal String
