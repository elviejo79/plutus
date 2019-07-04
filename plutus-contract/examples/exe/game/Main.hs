-- | Contract interface for the guessing game
module Main where

import           Control.Monad                     (foldM, void)
import qualified Data.Aeson                        as Aeson
import qualified Data.ByteString.Lazy.Char8        as BSL
import           Data.Foldable                     (traverse_)
import           Examples.Game                     (game, guessTrace, lockTrace)
import           Language.Plutus.Contract.Emulator (ContractTrace, EmulatorAction, execTrace)
import           Language.Plutus.Contract.Servant  (Request (..), Response (..), contractApp, initialResponse,
                                                    runUpdate)
import qualified Language.Plutus.Contract.State    as State
import           Network.Wai.Handler.Warp          (run)
import           System.Environment                (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []                -> run 8080 (contractApp game)
    ["trace","lock"]  -> printTrace lockTrace
    ["trace","guess"] -> printTrace guessTrace
    _                 -> putStrLn "call with [trace lock|trace guess] to print traces"

printTrace :: ContractTrace EmulatorAction () () -> IO ()
printTrace ctr = void (foldM go init events) where
  events = execTrace game ctr
  init = initialResponse game
  go previous e = do
    let st = newState previous
        newRequest = Request { oldState = st, event = e }
    BSL.putStrLn (Aeson.encode newRequest)
    either error pure (runUpdate game newRequest)

--curl -XPOST -d @body.json -H "Content-Type: application/json" localhost:8080/run