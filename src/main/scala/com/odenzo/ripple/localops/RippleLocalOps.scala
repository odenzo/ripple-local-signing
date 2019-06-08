package com.odenzo.ripple.localops

import cats._
import cats.data._
import io.circe.JsonObject
import scribe.Logging

import com.odenzo.ripple.localops.impl.utils.ByteUtils
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.impl.{Sign, SignFor, Verify}
import com.odenzo.ripple.localops.models.SigningKey

trait RippleLocalOps extends Logging {

  /** This is the recommended programmatic API for Local Signing. The TxBlob response is
    * the only item needed for subsequent `submit` to XRPL server.
    *
    * @param tx_json
    * @param signingKey
    *
    * @return TxBlob in Hex form suitable for submitting to Rippled XRPL
    */
  def signTxn(tx_json: JsonObject, signingKey: SigningKey): Either[AppError, String] = {
    for {
      sig    <- Sign.signToTxnSignature(tx_json, signingKey)
      txblob <- Sign.createSignedTxBlob(tx_json, sig)
      txblobHex = ByteUtils.bytes2hex(txblob)
    } yield txblobHex
  }

  def signToTxnSignature(tx_json: JsonObject, signingKey: SigningKey): Either[AppError, String] = {
    Sign.signToTxnSignature(tx_json, signingKey).map(_.hex)
  }

  /** Signs a txn with the key and returns the tx blob and transaction hash for inclusion in Submit Request
    * Very little validation or error checking is done, and no enrichment.
    * On submissions of the resulting txblob to server final validation is done.
    * The hash is not really needed in most cases, and the submit hash is what is used to track the txn.
    *
    * @param tx_json Transaction subsection. No fields will be supplemented, Sequence and Fee should be filled.
    *
    * @return (tx_blob, hash)  in hex format. Note that hash of txn is just SHA512 of tx_blob bytes
    * */
  def signTxnWithHash(tx_json: JsonObject, signingKey: SigningKey): Either[AppError, (String, String)] = {
    for {
      sig    <- Sign.signToTxnSignature(tx_json, signingKey)
      txblob <- Sign.createSignedTxBlob(tx_json, sig)
      txblobHex = ByteUtils.bytes2hex(txblob)
      hashHex   = Sign.createResponseHashHex(txblob)
    } yield (txblobHex, hashHex)

  }

  def signFor(tx_json: JsonObject, key: SigningKey, signingAcct: String): Either[Throwable, JsonObject] = {
    SignFor.signFor(tx_json, key, signingAcct)
  }

  /**
    * This is recommended API for verifying a Signature.
    * I have yet to run across a client side use-case for this.
    *
    * Takes a signed tx_json object and verified the TxnSignature
    * usign SigningPubKey
    *
    * @param tx_json
    *
    * @return true if verified correctly
    */
  def verify(tx_json: JsonObject): Either[AppError, Boolean] = {
    Verify.verifySigningResponse(tx_json)
  }

}
