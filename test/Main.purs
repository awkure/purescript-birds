module Test.Main where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Aviary.Birds 

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Test suite not yet implemented"
