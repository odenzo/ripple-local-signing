package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import scribe.Logging

import com.odenzo.ripple.bincodec.{EncodedSTObject, RippleCodecAPI}
import com.odenzo.ripple.localops.crypto.RippleFormatConverters
import com.odenzo.ripple.localops.crypto.core.HashOps
import com.odenzo.ripple.localops.handlers.{SignForRqRsHandler, SignRqRsHandler}
import com.odenzo.ripple.localops.utils.ByteUtils
import com.odenzo.ripple.localops.utils.caterrors.AppError

object RippleLocalOps extends Logging {

  /** This is the recommended programmatic API for Local Signing. The TxBlob response is
    * the only item needed for subsequent `submit` to XRPL server.
    *
    * @param tx_json
    * @param signingKey
    *
    * @return
    */
  def signToTxnBlob(tx_json: JsonObject, signingKey: SigningKey): Either[AppError, (String, String)] = {
    for {
      sig    ← Signer.signToTxnSignature(tx_json, signingKey)
      txblob ← Signer.createSignedTxBlob(tx_json, sig)
      txblobHex = ByteUtils.bytes2hex(txblob)
      hash      = HashOps.sha512(txblob)
      hashHex   = ByteUtils.bytes2hex(hash)
    } yield (txblobHex, hashHex)
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

  /** Pack a key into internal format. Parameters per WalletProposeRs */
  def packSigningKey(master_seed_hex: String, key_type: KeyType): Either[AppError, SigningKey] = {
    Signer.preCalcKeys(master_seed_hex, key_type)
  }

  /** Pack a key into internal format. Parameters per WalletProposeRs */
  def packSigningKeyFromB58(master_seed: String, key_type: KeyType): Either[AppError, SigningKey] = {
    RippleFormatConverters
      .convertBase58Check2hex(master_seed)
      .flatMap(packSigningKey(_, key_type))
  }

  /** Pack a key into internal format. Parameters per WalletProposeRs */
  def packSigningKeyFromRFC1751(master_key: String, key_type: KeyType): Either[AppError, SigningKey] = {
    RippleFormatConverters
      .convertMasterKey2masterSeedHex(master_key)
      .flatMap(packSigningKey(_, key_type))
  }

  def sign(signRq: JsonObject): JsonObject = {

    SignRqRsHandler.processSignRequest(signRq) match {
      case Left(v)  ⇒ v
      case Right(v) ⇒ v
    }

  }

  /**
    * Mimics a SignRq as much as possible. The SignRs is not returned, instead
    * just the TxBlob for use in the SubmitRq
    * Note that the Fee should already be specified, also all the paths.
    *
    * This is for backward compatiability, signToTxnBlob is preferred method for speed
    *
    */
  def signFor(signRq: JsonObject): JsonObject = {

    SignForRqRsHandler.signFor(signRq) match {
      case Left(v)  ⇒ v
      case Right(v) ⇒ v
    }

  }

  def signToTxnSignature(tx_json: JsonObject, signingKey: SigningKey): Either[AppError, TxnSignature] = {
    Signer.signToTxnSignature(tx_json, signingKey)
  }

  /**
    * Expects a top level JsonObject representing a JSON document
    * that would be sent to rippled server. All isSerializable fields serialized.
    *
    * @param jsonObject
    *
    * @return Hex string representing the serialization in total.
    */
  def binarySerialize(jsonObject: JsonObject): Either[AppError, EncodedSTObject] = {
    RippleCodecAPI.binarySerialize(jsonObject).leftMap(AppError.wrapCodecError)
  }

  /**
    * Expects a top level JsonObject representing a transaction
    * that would be sent to rippled server. All isSigningField fields serialized.
    * This and binarySerialize and the only two top level user
    * FIXME: I am guessing this is the whole transaction because fee_multi_max and other important stuff in top
    * level
    *
    * @param tx_json
    */
  def binarySerializeForSigning(tx_json: JsonObject): Either[AppError, EncodedSTObject] = {
    logger.trace("Serializing for txnscenarios")
    RippleCodecAPI.binarySerializeForSigning(tx_json).leftMap(AppError.wrapCodecError)
  }

  def serialize(tx_json: JsonObject): Either[AppError, Array[Byte]] = {
    RippleCodecAPI.serializedTxBlob(tx_json).leftMap(AppError.wrapCodecError)
  }

  /**
    *
    * @param tx_json Fully formed tx_json with all auto-fillable fields etc.
    * @return Byte Array representing the serialized for signing txn. Essentially TxBlob
    */
  def serializeForSigning(tx_json: JsonObject): Either[AppError, Array[Byte]] = {
    RippleCodecAPI.signingTxBlob(tx_json).leftMap(AppError.wrapCodecError)
  }
}
