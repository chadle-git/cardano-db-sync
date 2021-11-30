{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Mock.Forging.Tx.Shelley where


import           Cardano.Prelude

import qualified Data.Maybe.Strict as Strict
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Coin
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Shelley.TxBody hiding (TxIn)
import           Cardano.Ledger.Shelley.Tx

import           Ouroboros.Consensus.Cardano.Block (LedgerState)
import           Ouroboros.Consensus.Shelley.Eras (ShelleyEra, StandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

import           Cardano.Slotting.Slot (SlotNo (..))

import           Cardano.Mock.Forging.Tx.Generic
import           Cardano.Mock.Forging.Types


mkPaymentTx :: UTxOIndex -> UTxOIndex -> Integer -> Integer
            -> LedgerState (ShelleyBlock (ShelleyEra StandardCrypto))
            -> Either ForgingError (Tx (ShelleyEra StandardCrypto))
mkPaymentTx inputIndex outputIndex amount fees st = do
    (inputPair, _) <- resolveUTxOIndex inputIndex st
    (outputPair, _ ) <- resolveUTxOIndex outputIndex st
    let input = Set.singleton $ fst inputPair
        TxOut addr _ = snd outputPair
        output = TxOut addr (Coin amount)
        TxOut addr' (Coin inputValue) = snd inputPair
        change = TxOut addr' $ Coin (inputValue - amount - fees)

    Right $ mkSimpleTx $ consPaymentTx input (StrictSeq.fromList [output, change]) (Coin fees)

mkSimpleTx :: TxBody (ShelleyEra StandardCrypto) -> Tx (ShelleyEra StandardCrypto)
mkSimpleTx txBody = Tx
    txBody
    mempty
    (maybeToStrictMaybe Nothing)

consPaymentTx :: Set (TxIn (Crypto (ShelleyEra StandardCrypto)))
              -> StrictSeq (TxOut (ShelleyEra StandardCrypto))
              -> Coin -> TxBody (ShelleyEra StandardCrypto)
consPaymentTx ins outs fees =
    TxBody
      ins
      outs
      mempty
      (Wdrl mempty)
      fees
      (SlotNo 1000000000) -- TODO ttl
      Strict.SNothing
      Strict.SNothing

