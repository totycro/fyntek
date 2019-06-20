module Main where

import Prelude (Unit, show)
import Effect
import Effect.Console (log)

main :: Effect Unit
main = do
  log (show 3)
