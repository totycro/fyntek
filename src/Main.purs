module Main where

import Prelude (Unit, show, bind)
import Effect
import Effect.Console (log)

import Effect.Aff (launchAff)
import Effect.Class (liftEffect)

import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  fileContents <- readTextFile UTF8 "input.csv"
  log fileContents
