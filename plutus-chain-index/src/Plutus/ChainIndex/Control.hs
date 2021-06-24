{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
{-| A freer effect for updating the chain index state with new transactions.
-}
module Plutus.ChainIndex.Control(
    ChainIndexControlEffect(..)
    , insertBlock
    , rollback
    , collectGarbage
    ) where

import           Control.Monad.Freer.TH  (makeEffect)
import           Ledger                  (Slot)
import           Plutus.ChainIndex.Tx    (ChainIndexTx)
import           Plutus.ChainIndex.Types (BlockId)

data ChainIndexControlEffect r where

    -- | Insert a new block into the chain index
    InsertBlock :: Slot -> BlockId -> [ChainIndexTx] -> ChainIndexControlEffect ()

    -- | Roll back to a previous state
    Rollback    :: Slot -> BlockId -> ChainIndexControlEffect ()

    -- | Delete all data that is not covered by current UTXOs.
    CollectGarbage :: ChainIndexControlEffect ()

makeEffect ''ChainIndexControlEffect
