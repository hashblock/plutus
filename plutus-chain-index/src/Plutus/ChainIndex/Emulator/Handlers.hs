{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Plutus.ChainIndex.Emulator.Handlers(
    handleQuery
    , handleControl
    , ChainIndexEmulatorState
    ) where

import           Control.Lens                         (at, ix, makeLenses, over, preview, to, view)
import           Control.Monad.Freer                  (Eff, Member, type (~>))
import           Control.Monad.Freer.Error            (Error, throwError)
import           Control.Monad.Freer.State            (State, gets, modify)
import           Data.Default                         (Default (..))
import           Data.FingerTree                      (Measured (..))
import qualified Data.Set                             as Set
import           Ledger                               (TxOutRef (..))
import           Plutus.ChainIndex.Effects            (ChainIndexControlEffect (..), ChainIndexQueryEffect (..))
import           Plutus.ChainIndex.Emulator.DiskState (DiskState, addressMap, dataMap, mintingPolicyMap, txMap,
                                                       validatorMap)
import qualified Plutus.ChainIndex.Emulator.DiskState as DiskState
import           Plutus.ChainIndex.Tx                 (citxOutputs)
import           Plutus.ChainIndex.Types              (Tip, pageOf)
import           Plutus.ChainIndex.UtxoState          (UtxoIndex, isUnspentOutput, tip, tubUnspentOutputs,
                                                       usTxUtxoBalance)
import qualified Plutus.ChainIndex.UtxoState          as UtxoState

data ChainIndexEmulatorState =
    ChainIndexEmulatorState
        { _diskState :: DiskState
        , _utxoIndex :: UtxoIndex
        }

makeLenses ''ChainIndexEmulatorState

handleQuery ::
    forall effs.
    ( Member (State ChainIndexEmulatorState) effs
    , Member (Error ChainIndexError) effs
    ) => ChainIndexQueryEffect
    ~> Eff effs
handleQuery = \case
    DatumFromHash h -> gets (view $ diskState . dataMap . at h)
    ValidatorFromHash h -> gets (view $ diskState . validatorMap . at h)
    MintingPolicyFromHash h -> gets (view $ diskState . mintingPolicyMap . at h)
    TxOutFromRef TxOutRef{txOutRefId, txOutRefIdx} -> gets @ChainIndexEmulatorState (preview $ diskState . txMap . ix txOutRefId . citxOutputs . ix (fromIntegral txOutRefIdx))
    TxFromTxId i -> gets (view $ diskState . txMap . at i)
    UtxoSetMembership r -> do
        utxoState <- gets (measure . view utxoIndex)
        case tip utxoState of
            Nothing -> throwError NoTip
            Just tp -> pure (tp, isUnspentOutput r utxoState)
    UtxoSetAtAddress cred -> gets (pageOf def .  view (utxoIndex . to measure . usTxUtxoBalance . tubUnspentOutputs))

handleControl ::
    forall effs.
    Member (State ChainIndexEmulatorState) effs
    => ChainIndexControlEffect
    ~> Eff effs
handleControl = \case
    AppendBlock tip transactions -> do
        modify @ChainIndexEmulatorState
            ( over diskState (mappend $ foldMap DiskState.fromTx transactions)
            . over utxoIndex (mappend $ UtxoState.fromBlock tip transactions))
            -- TODO: 1. Check if the new tip is later than the last one
            --       2. (maybe) add an "insert" operation that inserts it at
            --          the right place (instead of just appending)
    Rollback tip -> undefined
    CollectGarbage -> undefined

data ChainIndexError =
        NoTip -- ^ Querying a chain index that has not been synchronised
        | InsertOldTip { lastTip :: Tip, insertTip :: Tip } -- ^ Attempt to insert a block with a tip that is in the past
