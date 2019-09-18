package com.odenzo.ripple.localops

import io.circe.{Json, ParsingFailure}

import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.localops.impl.crypto.RippleFormatConverters
import com.odenzo.ripple.localops.impl.utils.ByteUtils
import com.odenzo.ripple.localops.impl.{Sign, SignFor, Verification, WalletGenerator}
import com.odenzo.ripple.localops.models.{KeyType, SigningKey, WalletProposeResult}

/**
  * This is the simple API to use the Ripple Local Operations. See RippleLocalOps for a superset of this API
  * that may be handy for existing code bases.
  */
trait RippleLocalAPI {

  /** This is the recommended programmatic API for Local Signing. The TxBlob response is
    * the only item needed for subsequent `submit` to XRPL server.
    * This is for signle signing.
    * @param tx_json    TxJson fully formed (auto-filled) to submit
    * @param signingKey Prepackaged signing key.
    *
    * @return TxBlob in Hex form suitable for submitting to Rippled XRPL
    */
  def signTxn(tx_json: Json, signingKey: SigningKey): Either[LocalOpsError, String] = {
    for {
      sig    <- Sign.signToTxnSignature(tx_json, signingKey)
      txblob <- Sign.createSignedTxBlob(tx_json, sig)
      txblobHex = ByteUtils.bytes2hex(txblob)
    } yield txblobHex
  }

  /** Forget the use case for this, perhaps remove */
  def signToTxnSignature(tx_json: Json, signingKey: SigningKey): Either[LocalOpsError, String] = {
    Sign.signToTxnSignature(tx_json, signingKey).map(_.hex)
  }

  /**
    * Multisignes a tx_json object with one key, adds Signer to existing Signers or creates Signers with exactly
    * Signer
    * @param tx_json
    * @param key
    * @param signingAcct
    * @return An updated tx_json with addition Signer in the Signers field (correctly sorted)
    */
  def multisign(tx_json: Json, key: SigningKey, signingAcct: String): Either[Throwable, Json] = {
    SignFor.signFor(tx_json, key, signingAcct)
  }

  /** If you signFor the base tx_json N times you get N signFor responses each with one Signer.
    * This merges those all together into something that can be submitted. Its the full response with embedded
    * tx_json updates
    * @param rs of tx_json objects with Signers fields and Signer objects to merge
    */
  def multisignMerge(rs: List[Json]): Either[LocalOpsError, Json] = {
    SignFor.mergeMultipleTxJsonResponses(rs)
  }

  /** Once all the signatures on in a tx_json structure this is used to calcaulre the tx_blob for submittsion
    * No Additional signing is done.
    **/
  def multisignedToTxBlob(tx_json: Json): Either[LOpException, String] = {
    SignFor.generateTxBlob(tx_json)

  }

  /**
    * This is recommended API for verifying a Signature.
    * I have yet to run across a client side use-case for this.
    *
    * Takes a signed tx_json object and verified the TxnSignature
    * usign SigningPubKey .
    *
    * This is updated to deal with multisigned and single signed.
    *
    * @param tx_json
    *
    * @return true if verified correctly
    */
  def verify(tx_json: Json): Either[LocalOpsError, Boolean] = {
    Verification.verifySignedResponse(tx_json)
  }

  /**
    * Generates two sets of keys, but doesn't activate them in any way.
    *
    * @return Master and Regular KeyPair based on random seed.
    */
  def generateAccountKeys(key_type: String): Either[LocalOpsError, WalletProposeResult] = {

    for {
      keyType <- KeyType.fromText(key_type).leftMap(e => LocalOpsError(e.error_message.getOrElse("Bad KeyType")))
      master  <- WalletGenerator.generateWallet(keyType)
    } yield (master)
  }

  /**
    * Wrapws keypair in SigningKey for optimzed usage. Typically client cache's this.
    *
    * @param master_seed_hex The master seed of an keypair, either master or regular keypair.
    * @param key_type     ED25519 or SECP2561K1
    *
    * @return An opaque structure that should not be relied on for anyway.
    *         Clients should persist keys using the master_seed in an encrypted vault.
    */
  def packSigningKey(master_seed_hex: String, key_type: String): Either[LocalOpsError, SigningKey] = {
    KeyType
      .fromText(key_type)
      .leftMap(e => LocalOpsError(s"Bad Input for key_type ${key_type} + ${e.error_message}"))
      .flatMap(Sign.preCalcKeys(master_seed_hex, _))
  }

  /**
    * Wrapws keypair in SigningKey for optimzed usage. Typically client cache's this.
    *
    * @param master_seed The master seed of an keypair, either master or regular keypair.
    * @param key_type     ED25519 or SECP2561K1
    *
    * @return An opaque structure that should not be relied on for anyway.
    *         Clients should persist keys using the master_seed in an encrypted vault.
    */
  def packSigningKeyFromB58(master_seed: String, key_type: String): Either[LocalOpsError, SigningKey] = {
    RippleFormatConverters
      .convertBase58Check2hex(master_seed)
      .flatMap(packSigningKey(_, key_type))
  }

  /** Pack a key into internal format. Parameters per WalletProposeRs */
  def packSigningKeyFromRFC1751(master_key: String, key_type: String): Either[LocalOpsError, SigningKey] = {
    RippleFormatConverters
      .convertMasterKey2masterSeedHex(master_key)
      .flatMap(packSigningKey(_, key_type))
  }

  def precomputeSigningKey(master_seed: String, key_type: String): Either[LocalOpsError, SigningKey] = {
    packSigningKeyFromB58(master_seed, key_type)
  }

  /**
    *
    * @param json Well formed Json
    * @return Parsed json that can be used in other API methods.
    * @throws io.circe.ParsingFailure  exception if Json is malformed
    */
  def parseJsonUNSAFE(json: String): Json = {
    io.circe.parser.parse(json) match {
      case Right(j)  => j
      case Left(err) => throw err
    }
  }

}

object RippleLocalAPI extends RippleLocalAPI
