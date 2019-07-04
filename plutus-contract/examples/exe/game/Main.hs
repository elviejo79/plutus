-- | Contract interface for the guessing game
module Main where

import           Examples.Game                (game, guessTrace, lockTrace)
import qualified Language.Plutus.Contract.App as App

main :: IO ()
main = App.runWithTraces game
          [ ("lock", lockTrace)
          , ("guess", guessTrace)]

--curl -XPOST -d @out.json -H "Content-Type: application/json" localhost:8080/run
