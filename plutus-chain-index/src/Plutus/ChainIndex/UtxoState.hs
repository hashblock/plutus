{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TemplateHaskell       #-}
{-| The UTXO state, kept in memory by the chain index.
-}
module Plutus.ChainIndex.UtxoState(
    UtxoState(..)
    , usTxUtxoBalance
    , usTip
    , TxUtxoBalance(..)
    , tubUnspentOutputs
    , tubUnmatchedSpentInputs
    , UtxoIndex
    , fromBlock
    , fromTx
    , isUnspentOutput
    , tip
    , unspentOutputs
    ) where

import           Control.Lens            (makeLenses, view)
import           Data.FingerTree         (FingerTree, Measured (..))
import qualified Data.FingerTree         as FT
import           Data.Monoid             (Last (..))
import           Data.Semigroup.Generic  (GenericSemigroupMonoid (..))
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           GHC.Generics            (Generic)
import           Ledger                  (TxIn (txInRef), TxOutRef (..))
import           Plutus.ChainIndex.Tx    (ChainIndexTx (..), txOutRefs)
import           Plutus.ChainIndex.Types (Tip)

-- | The effect of a transaction (or a number of them) on the utxo set.
data TxUtxoBalance =
    TxUtxoBalance
        { _tubUnspentOutputs       :: Set TxOutRef -- ^ Outputs newly added by the transaction(s)
        , _tubUnmatchedSpentInputs :: Set TxOutRef -- ^ Outputs spent by the transaction(s)
        }
        deriving stock (Eq, Show, Generic)

makeLenses ''TxUtxoBalance

instance Semigroup TxUtxoBalance where
    l <> r =
        TxUtxoBalance
            { _tubUnspentOutputs       = _tubUnspentOutputs r <> (_tubUnspentOutputs l `Set.difference` _tubUnmatchedSpentInputs r)
            , _tubUnmatchedSpentInputs = (_tubUnmatchedSpentInputs r `Set.difference` _tubUnspentOutputs l) <> _tubUnmatchedSpentInputs l
            }

instance Monoid TxUtxoBalance where
    mappend = (<>)
    mempty = TxUtxoBalance mempty mempty

-- | UTXO / ledger state, kept in memory. We are only interested in the UTXO set, everything else is stored
--   on disk. This is OK because we don't need to validate transactions when they come in.
data UtxoState =
    UtxoState
        { _usTxUtxoBalance :: TxUtxoBalance
        , _usTip           :: Last Tip -- ^ Tip of our chain sync client
        }
        deriving stock (Eq, Show, Generic)
        deriving (Semigroup, Monoid) via (GenericSemigroupMonoid UtxoState)

makeLenses ''UtxoState

fromTx :: ChainIndexTx -> TxUtxoBalance
fromTx tx@ChainIndexTx{_citxInputs} =
    TxUtxoBalance
        { _tubUnspentOutputs = Set.fromList $ fmap snd $ txOutRefs tx
        , _tubUnmatchedSpentInputs = Set.mapMonotonic txInRef _citxInputs
        }

type UtxoIndex = FingerTree UtxoState UtxoState
instance Measured UtxoState UtxoState where
    measure = id

-- | Whether a 'TxOutRef' is a member of the UTXO set (ie. unspent)
isUnspentOutput :: TxOutRef -> UtxoState -> Bool
isUnspentOutput r = Set.member r . view (usTxUtxoBalance . tubUnspentOutputs)

tip :: UtxoState -> Maybe Tip
tip = getLast . view usTip

-- | The UTXO set
unspentOutputs :: UtxoState -> Set TxOutRef
unspentOutputs = view (usTxUtxoBalance . tubUnspentOutputs)

-- | 'UtxoIndex' for a single block
fromBlock :: Tip -> [ChainIndexTx] -> UtxoIndex
fromBlock tip transactions =
    FT.singleton
    $ UtxoState
            { _usTxUtxoBalance = foldMap fromTx transactions
            , _usTip           = Last (Just tip)
            }
