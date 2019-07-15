package com.odenzo.ripple.localops

import io.circe.JsonObject

import com.odenzo.ripple.localops.utils.caterrors.AppError

/**
  * This is the simple API to use the Ripple Local Operations. See RippleLocalOps for a superset of this API
  * that may be handy for existing code bases.
  */
trait LocalOpsAPI {

  /**
    * Generates two sets of keys, but doesn't activate them in any way.
    * @return Master and Regular KeyPair based on random seed.
    */
  def generateKeys(): Either[AppError, (WalletProposeResult, WalletProposeResult)] = {
    for {
      master  <- WalletGenerator.generateWallet()
      regular ← WalletGenerator.generateWallet()
    } yield (master, regular)
  }

  /**
    * Wrapws keypair in SigningKey for optimzed usage. Typically client cache's this.
    * @param master_seed The master seed of an keypair, either master or regular keypair.
    * @param keyType ED25519 or SECP2561K1
    * @return An opaque structure that should not be relied on for anyway.
    *                            Clients should persist keys using the master_seed in an encrypted vault.
    */
  def precomputeSigningKey(master_seed: String, keyType: KeyType = ED25519): Either[AppError, SigningKey] = {

    RippleLocalOps.packSigningKeyFromB58(master_seed, keyType)
  }

  /** Signs a txn with the key and returns the tx blob and transaction hash for inclusion in Submit Request
    * Very little validation or error checking is done, and no enrichment.
    * On submissions of the resulting txblob to server final validation is done.
    *  @param tx_json Transaction subsection. No fields will be supplemented, Sequence and Fee should be filled.
    *  @return (tx_blob, hash)  in hex format. Note that hash of txn is just SHA512 of tx_blob bytes
    **/
  def signTxn(tx_json: JsonObject, signWith: SigningKey): Either[AppError, (String, String)] = {
    RippleLocalOps.signToTxnBlob(tx_json, signWith)
  }

}

object LocalOpsAPI extends LocalOpsAPI
