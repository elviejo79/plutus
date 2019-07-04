{-# LANGUAGE FlexibleContexts #-}
-- | Testing contracts with HUnit
module Spec.HUnit(
      module X
    , TracePredicate
    , Spec.HUnit.not
    , endpointAvailable
    , interestingAddress
    , assertResult
    , tx
    , anyTx
    , walletFundsChange
    , waitingForSlot
    -- * Checking predicates
    , checkPredicate
    ) where

import           Control.Lens                         (at, to, (^.), folded)
import           Control.Monad.State                  (runStateT)
import           Control.Monad.Writer
import           Data.Foldable                        (toList)
import           Data.Functor.Contravariant           (Predicate (..))
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe)
import qualified Data.Set                             as Set
import qualified Test.Tasty.HUnit                     as HUnit
import           Test.Tasty.Providers                 (TestTree)

import           Language.Plutus.Contract.Contract    as Con
import           Language.Plutus.Contract.Hooks       (Hooks (..))
import qualified Language.Plutus.Contract.Hooks       as Hooks
import           Language.Plutus.Contract.Transaction (UnbalancedTx)

import qualified Ledger.Ada                           as Ada
import           Ledger.Slot                          (Slot)
import           Ledger.Tx                            (Address)
import           Ledger.Value                         (Value)
import qualified Ledger.Value                         as V
import           Wallet.Emulator                      (EmulatorAction, Wallet)
import qualified Wallet.Emulator                      as EM

import           Language.Plutus.Contract.Emulator    as X

type TracePredicate a = InitialDistribution -> Predicate (ContractTraceResult a)

hooks :: Wallet -> ContractTraceResult a -> Hooks
hooks w rs =
    let evts = rs ^. ctrTraceState . ctsEvents . at w . folded . to toList
        con  = rs ^. ctrTraceState . ctsContract
    in execContract evts con

not :: TracePredicate a -> TracePredicate a
not p a = Predicate $ \b -> Prelude.not (getPredicate (p a) b)

checkPredicate
    :: String
    -> ContractPrompt Maybe a
    -> TracePredicate a
    -> ContractTrace EmulatorAction a ()
    -> TestTree
checkPredicate nm con predicate action =
    HUnit.testCase nm $
        case runTrace con action of
            (Left err, _) ->
                HUnit.assertFailure $ "EmulatorAction failed. " ++ show err
            (Right (_, st), ms) ->
                let dt = ContractTraceResult ms st in
                HUnit.assertBool nm (getPredicate (predicate defaultDist) dt)

endpointAvailable :: Wallet -> String -> TracePredicate a
endpointAvailable w nm _ = Predicate $ \r ->
    nm `Set.member` Hooks.activeEndpoints (hooks w r)

interestingAddress :: Wallet -> Address -> TracePredicate a
interestingAddress w addr _ = Predicate $ \r ->
        addr `Set.member` Hooks.addresses (hooks w r)

tx :: Wallet -> (UnbalancedTx -> Bool) -> TracePredicate a
tx w flt _ = Predicate $ \r ->
    any flt (Hooks.transactions (hooks w r))

waitingForSlot :: Wallet -> Slot -> TracePredicate a
waitingForSlot w sl _ = Predicate $ \r ->
    Just sl == Hooks.nextSlot (hooks w r)

anyTx :: Wallet -> TracePredicate a
anyTx w = tx w (const True)

assertResult :: Wallet -> (Maybe a -> Bool) -> TracePredicate a
assertResult w p _ = Predicate $ \rs ->
    let evts = rs ^. ctrTraceState . ctsEvents . at w . folded . to toList
        con  = rs ^. ctrTraceState . ctsContract
        result = fst (runWriter (runStateT (runContract con) evts))
    in p (fst result)

walletFundsChange :: Wallet -> Value -> TracePredicate a
walletFundsChange w dlt initialDist = Predicate $
    \ContractTraceResult{_ctrEmulatorState = st} ->
        let initialValue = foldMap Ada.toValue (Map.fromList initialDist ^. at w)
            finalValue   = fromMaybe mempty (EM.fundsDistribution st ^. at w)
        in initialValue `V.plus` dlt == finalValue
