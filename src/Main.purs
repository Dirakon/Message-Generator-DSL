module Main where

import Prelude
import Data.Either (Either(..))
import Data.List.Lazy (Step(..), step)
import Data.String (joinWith)
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Evaluate (evaluate)
import Format (format)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Parse (parse)

main :: Effect Unit
main =
  launchAff_
    $ do
        text <- readTextFile UTF8 "bot.example.txt"
        let
          evaluatedCode = (format >>> parse >>> evaluate) text
        let
          textToOutput = case evaluatedCode of
            V (Left errors) -> (joinWith "\n" errors)
            V (Right res) -> res'
              where
              res' = case step res of
                Nil -> "No message can be generated!"
                Cons mes _ -> mes
        writeTextFile UTF8 "out.txt" textToOutput
