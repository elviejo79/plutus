{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- | A futures contract in Plutus. This example illustrates three concepts.
--   1. Maintaining a margin (a kind of deposit) during the duration of the contract to protect against breach of contract (see note [Futures in Plutus])
--   2. Using oracle values to obtain current pricing information (see note [Oracles] in Language.PlutusTx.Coordination.Contracts)
--   3. Using the redeemer script to model actions that the participants in the contract may take.
module Language.PlutusTx.Coordination.Contracts.Future(
    -- * Data types
    Future(..),
    FutureData(..),
    FutureRedeemer(..),
    -- * Actions
    initialise,
    settle,
    settleEarly,
    adjustMargin,
    -- * Script
    validatorScript,
    mkValidator
    ) where

import           Control.Monad                (void)
import           Control.Monad.Error.Class    (MonadError (..))
import           Data.Maybe                   (maybeToList)
import qualified Data.Set                     as Set
import           GHC.Generics                 (Generic)
import           Language.PlutusTx.Prelude
import qualified Language.PlutusTx            as PlutusTx
import           Ledger                       (DataScript (..), Slot(..), PubKey, TxOutRef, ValidatorScript (..), scriptTxIn, scriptTxOut)
import qualified Ledger                       as Ledger
import qualified Ledger.Interval              as Interval
import           Ledger.Validation            (OracleValue (..), PendingTx (..), PendingTxOut (..))
import qualified Ledger.Validation            as Validation
import qualified Ledger.Ada                   as Ada
import           Ledger.Ada                   (Ada)
import qualified Wallet                       as W
import           Wallet                       (WalletAPI (..), WalletAPIError, throwOtherError, createTxAndSubmit, defaultSlotRange)

{- note [Futures in Plutus]

A futures contract ("future") is an agreement to change ownership of an
asset at a certain time (the delivery time) for an agreed price (the forward
price). The time of the transfer, and the price, are fixed at the beginning of the contract.

On the mockchain we only have one type of asset (namely, Ada coin value),
so we simply exchange the difference in price between the forward price and the
actual price. This is called a "cash settlement".

The agreement involves two parties, a buyer (long position) and a seller (short
position). At the delivery time the actual price of the asset (spot price) is
quite likely different from the forward price. If the spot price is higher than
the forward price, then the seller transfers the difference to the buyer. If
the spot price is lower than the forward price, then the buyer transfers money
to the seller. In either case there is a risk that the payer does not meet their
obligation (by simply not paying). To protect against this risk, the contract
includes a kind of deposit called "margin".

Each party deposits an initial margin. If the price moves against the seller,
then the seller has to top up their margin periodically (in our case, once each
block). Likewise, if it moves against the buyer then the buyer has to top up
their margin. If either party fails to make a margin payment then the contract
will be settled early.

The current value of the underlying asset is determined by an oracle. See note
[Oracles] in Language.PlutusTx.Coordination.Contracts.

-}

-- | Initialise the futures contract by paying the initial margin.
--
initialise :: (
    MonadError WalletAPIError m,
    WalletAPI m)
    => PubKey
    -- ^ Identity of the holder of the long position
    -> PubKey
    -- ^ Identity of the holder of the short position
    -> Future
    -> m ()
initialise long short f = do
    let
        im = futureInitialMargin f
        o  = scriptTxOut (Ada.toValue im) (validatorScript f) ds
        ds = DataScript $ Ledger.lifted $ FutureData long short im im

    (payment, change) <- createPaymentWithChange (Ada.toValue im)
    void $ createTxAndSubmit defaultSlotRange payment (o : maybeToList change)

-- | Close the position by extracting the payment
settle :: (
    MonadError WalletAPIError m,
    WalletAPI m)
    => [TxOutRef]
    -> Future
    -> FutureData
    -> OracleValue Ada
    -> m ()
settle refs ft fd ov = do
    let
        delDate = futureDeliveryDate ft
        forwardPrice = futureUnitPrice ft
        OracleValue _ _ spotPrice = ov
        delta = Ada.multiply (Ada.fromInt $ futureUnits ft) (Ada.minus spotPrice forwardPrice)
        longOut = Ada.toValue (Ada.plus (futureDataMarginLong fd) delta)
        shortOut = Ada.toValue (Ada.minus (futureDataMarginShort fd) delta)
        red = Ledger.RedeemerScript $ Ledger.lifted $ Settle ov
        outs = [
            Ledger.pubKeyTxOut longOut (futureDataLong fd),
            Ledger.pubKeyTxOut shortOut (futureDataShort fd)
            ]
        inp = (\r -> scriptTxIn r (validatorScript ft) red) <$> refs
        range = W.intervalFrom delDate
    void $ createTxAndSubmit range (Set.fromList inp) outs

-- | Settle the position early if a margin payment has been missed.
settleEarly :: (
    MonadError WalletAPIError m,
    WalletAPI m)
    => [TxOutRef]
    -> Future
    -> FutureData
    -> OracleValue Ada
    -> m ()
settleEarly refs ft fd ov = do
    let totalVal = Ada.toValue (Ada.plus (futureDataMarginLong fd) (futureDataMarginShort fd))
        outs = [Ledger.pubKeyTxOut totalVal (futureDataLong fd)]
        inp = (\r -> scriptTxIn r (validatorScript ft) red) <$> refs
        red = Ledger.RedeemerScript $ Ledger.lifted $ Settle ov
    void $ createTxAndSubmit defaultSlotRange (Set.fromList inp) outs

adjustMargin :: (
    MonadError WalletAPIError m,
    WalletAPI m)
    => [TxOutRef]
    -> Future
    -> FutureData
    -> Ada
    -> m ()
adjustMargin refs ft fd vl = do
    pk <- ownPubKey
    (payment, change) <- createPaymentWithChange (Ada.toValue vl)
    fd' <- let fd''
                | pk == futureDataLong fd = pure $ fd { futureDataMarginLong  = Ada.plus vl (futureDataMarginLong fd)  }
                | pk == futureDataShort fd = pure $ fd { futureDataMarginShort = Ada.plus vl (futureDataMarginShort fd) }
                | otherwise = throwOtherError "Private key is not part of futures contrat"
            in fd''
    let
        red = Ledger.RedeemerScript $ Ledger.lifted AdjustMargin
        ds  = DataScript $ Ledger.lifted fd'
        o = scriptTxOut outVal (validatorScript ft) ds
        outVal = Ada.toValue (Ada.plus vl (Ada.plus (futureDataMarginLong fd) (futureDataMarginShort fd)))
        inp = Set.fromList $ (\r -> scriptTxIn r (validatorScript ft) red) <$> refs
    void $ createTxAndSubmit defaultSlotRange (Set.union payment inp) (o : maybeToList change)


-- | Basic data of a futures contract. `Future` contains all values that do not
--   change during the lifetime of the contract.
--
data Future = Future {
    futureDeliveryDate  :: Slot,
    futureUnits         :: Integer,
    futureUnitPrice     :: Ada,
    futureInitialMargin :: Ada,
    futurePriceOracle   :: PubKey,
    futureMarginPenalty :: Ada
    -- ^ How much a participant loses if they fail to make the required margin
    --   payments.
    } deriving Generic

-- | The current "state" of the futures contract. `FutureData` contains values
--   that may change during the lifetime of the contract. This is the data
--   script.
--
data FutureData = FutureData {
    futureDataLong        :: PubKey,
    -- ^ Holder of the long position (buyer)
    futureDataShort       :: PubKey,
    -- ^ Holder of the short position (seller)
    futureDataMarginLong  :: Ada,
    -- ^ Current balance of the margin account of the long position
    futureDataMarginShort :: Ada
    -- ^ Current balance of the margin account of the short position
    } deriving Generic

-- | Actions that either participant may take. This is the redeemer script.
data FutureRedeemer =
      AdjustMargin
    -- ^ Make a margin payment
    | Settle (OracleValue Ada)
    -- ^ Settle the contract
    deriving Generic

-- | Compute the required margin from the current price of the
--   underlying asset.
requiredMargin :: Future -> Ada -> Ada
requiredMargin Future{futureUnits=units, futureUnitPrice=unitPrice, futureMarginPenalty=pnlty} spotPrice =
    let
        delta  = Ada.multiply (Ada.fromInt units) (Ada.minus spotPrice unitPrice)
    in
        Ada.plus pnlty delta

{-# INLINABLE mkValidator #-}
mkValidator :: Future -> FutureData -> FutureRedeemer -> PendingTx -> Bool
mkValidator ft@Future{..} FutureData{..} r p@PendingTx{pendingTxOutputs=outs, pendingTxValidRange=range} =
    let

        isPubKeyOutput :: PendingTxOut -> PubKey -> Bool
        isPubKeyOutput o k = maybe False ((==) k) (Validation.pubKeyOutput o)

        --  | Check if a `PendingTxOut` is a public key output for the given pub. key and ada value
        paidOutTo :: Ada -> PubKey -> PendingTxOut -> Bool
        paidOutTo vl pk txo =
            let PendingTxOut vl' _ _ = txo
                adaVl' = Ada.fromValue vl'
            in
            isPubKeyOutput txo pk && vl == adaVl'

        verifyOracle :: OracleValue a -> (Slot, a)
        verifyOracle (OracleValue pk h t) =
            if pk == futurePriceOracle then (h, t) else error ()

    in case r of
            -- Settling the contract is allowed if any of three conditions hold:
            --
            -- 1. The `deliveryDate` has been reached. In this case both parties get what is left of their margin
            -- plus/minus the difference between spot and forward price.
            -- 2. The owner of the long position has failed to make a margin payment. In this case the owner of the short position gets both margins.
            -- 3. The owner of the short position has failed to make a margin payment. In this case the owner of the long position gets both margins.
            --
            -- In case (1) there are two payments (1 to each of the participants). In cases (2) and (3) there is only one payment.

            Settle ov ->
                let
                    spotPrice = snd (verifyOracle ov)
                    delta  = Ada.multiply (Ada.fromInt futureUnits) (Ada.minus spotPrice futureUnitPrice)
                    expShort = Ada.minus futureDataMarginShort delta
                    expLong  = Ada.plus futureDataMarginLong delta
                    slotvalid = Interval.member futureDeliveryDate range

                    canSettle =
                        case outs of
                            o1:o2:_ ->
                                let paymentsValid =
                                        (paidOutTo expShort futureDataShort o1 && paidOutTo expLong futureDataLong o2)
                                        || (paidOutTo expShort futureDataShort o2 && paidOutTo expLong futureDataLong o1)
                                in
                                    slotvalid && paymentsValid
                            o1:_ ->
                                let
                                    totalMargin = Ada.plus futureDataMarginShort futureDataMarginLong
                                    reqMargin   = requiredMargin ft spotPrice
                                    case2 = futureDataMarginLong < reqMargin
                                            && paidOutTo totalMargin futureDataShort o1

                                    case3 = futureDataMarginShort < reqMargin
                                            && paidOutTo totalMargin futureDataLong o1

                                in
                                    case2 || case3
                            _ -> False

                in
                    canSettle

            -- For adjusting the margin we simply check that the amount locked in the contract
            -- is larger than it was before.
            --
            AdjustMargin ->
                let
                    ownHash = fst (Validation.ownHashes p)
                    vl = Validation.adaLockedBy p ownHash
                in
                    vl > (futureDataMarginShort `Ada.plus` futureDataMarginLong)

validatorScript :: Future -> ValidatorScript
validatorScript ft = ValidatorScript $
    $$(Ledger.compileScript [|| mkValidator ||])
        `Ledger.applyScript`
            Ledger.lifted ft

PlutusTx.makeLift ''Future
PlutusTx.makeLift ''FutureData
PlutusTx.makeLift ''FutureRedeemer
