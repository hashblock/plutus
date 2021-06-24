{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-| The chain index' version of a transaction
-}
module Plutus.ChainIndex.Tx(
    ChainIndexTx(..)
    , fromOnChainTx
    ) where

import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           GHC.Generics (Generic)
import           Ledger       (Datum, DatumHash, MintingPolicy, MintingPolicyHash, OnChainTx (..), Redeemer (..),
                               RedeemerHash, SlotRange, Tx (..), TxId, TxIn (txInType), TxInType (..), TxOut, Validator,
                               ValidatorHash, datumHash, mintingPolicyHash, redeemerHash, txId, validatorHash)

data ChainIndexTx = ChainIndexTx {
    citxId              :: TxId,
    citxInputs          :: Set TxIn,
    citxOutputs         :: [TxOut],
    citxValidRange      :: !SlotRange,
    citxData            :: Map DatumHash Datum,
    citxRedeemers       :: Map RedeemerHash Redeemer,
    citxMintingPolicies :: Map MintingPolicyHash MintingPolicy,
    citxValidators      :: Map ValidatorHash Validator
    } deriving (Show, Eq, Generic)

fromOnChainTx :: OnChainTx -> ChainIndexTx
fromOnChainTx = \case
    Valid tx@Tx{txInputs, txOutputs, txValidRange, txData, txMintScripts} ->
        let (validatorHashes, otherDataHashes, redeemers) = validators txInputs in
        ChainIndexTx
            { citxId = txId tx
            , citxInputs = txInputs
            , citxOutputs = txOutputs
            , citxValidRange = txValidRange
            , citxData = txData <> otherDataHashes
            , citxRedeemers = redeemers
            , citxMintingPolicies = mintingPolicies txMintScripts
            , citxValidators = validatorHashes
            }
    Invalid tx@Tx{txCollateral, txValidRange, txData, txInputs, txMintScripts} ->
        let (validatorHashes, otherDataHashes, redeemers) = validators txInputs in
        ChainIndexTx
            { citxId = txId tx
            , citxInputs = txCollateral
            , citxOutputs = mempty
            , citxValidRange = txValidRange
            , citxData = txData <> otherDataHashes
            , citxRedeemers = redeemers
            , citxMintingPolicies = mintingPolicies txMintScripts
            , citxValidators = validatorHashes
            }

mintingPolicies :: Set MintingPolicy -> Map MintingPolicyHash MintingPolicy
mintingPolicies =
    let withHash mps = (mintingPolicyHash mps, mps) in
    Map.fromList . fmap withHash . Set.toList

validators :: Set TxIn -> (Map ValidatorHash Validator, Map DatumHash Datum, Map RedeemerHash Redeemer)
validators =
    let withHash (ConsumeScriptAddress val red dat) =
            ( Map.singleton (validatorHash val) val
            , Map.singleton (datumHash dat) dat
            , Map.singleton (redeemerHash red) red
            )
        withHash ConsumePublicKeyAddress    = mempty
    in foldMap (maybe mempty withHash . txInType) . Set.toList
