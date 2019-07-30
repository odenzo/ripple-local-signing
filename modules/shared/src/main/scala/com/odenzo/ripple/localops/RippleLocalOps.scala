package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import scribe.Logging

import com.odenzo.ripple.localops.impl.utils.ByteUtils
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.impl.{Signer, Verify, WalletGenerator}
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
      sig    <- Signer.signToTxnSignature(tx_json, signingKey)
      txblob <- Signer.createSignedTxBlob(tx_json, sig)
      txblobHex = ByteUtils.bytes2hex(txblob)
    } yield txblobHex
  }

  def signToTxnSignature(tx_json: JsonObject, signingKey: SigningKey): Either[AppError, String] = {
    Signer.signToTxnSignature(tx_json, signingKey).map(_.hex)
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
      sig    <- Signer.signToTxnSignature(tx_json, signingKey)
      txblob <- Signer.createSignedTxBlob(tx_json, sig)
      txblobHex = ByteUtils.bytes2hex(txblob)
      hashHex   = Signer.createResponseHashHex(txblob.toIndexedSeq) // This was just a Hash512
    } yield (txblobHex, hashHex)

  }

  def signFor(tx_json: JsonObject, key: SigningKey, signingAcct: String): Either[Throwable, JsonObject] = {
    Signer.signFor(tx_json, key, signingAcct)
  }

  /**
    * Takes a request style tx_json and a list of Siger fields and produces a tx_Json suitable for submission.
    * The hash field is not calculated for the result tx_json
    *
    * @param tx_json    tx_json, a copy of whose  Signers list will be replaced and hash field removed
    * @param signerList A list of Signer fields that will be combined in Signers and inserted into tx_json, hash added
    *
    * @return tx_json suitable for multisign submission (with no hash field)
    */
  def combineSigners(tx_json: JsonObject, signerList: List[JsonObject]): JsonObject = {
    val signers = Signer.combineSignerObjects(signerList)
    tx_json.remove("Signers").remove("hash").add("Signers", signers)

    // Note we are not recaulculating the tx_json hash
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
