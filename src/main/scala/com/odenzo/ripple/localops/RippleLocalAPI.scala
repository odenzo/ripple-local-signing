package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe.JsonObject
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

import com.odenzo.ripple.bincodec.RippleCodecAPI
import com.odenzo.ripple.bincodec.serializing.BinarySerializer
import com.odenzo.ripple.localops.crypto.{AccountFamily, RippleFormatConverters}
import com.odenzo.ripple.localops.utils.RBase58
import com.odenzo.ripple.localops.utils.caterrors.AppError

object RippleLocalAPI extends StrictLogging {

  /** Pack a key into internal format. Parameters per WalletProposeRs */
  def packSigningKey(master_seed_hex: String, key_type: String): Either[AppError, SigningKey] = {
    Signer.preCalcKeys(master_seed_hex, key_type)
  }

  /** Pack a key into internal format. Parameters per WalletProposeRs */
  def packSigningKeyFromB58(master_seed: String, key_type: String): Either[AppError, SigningKey] = {
    RippleFormatConverters
    .convertMasterSeedB582MasterSeedHex(master_seed)
    .flatMap(packSigningKey(_, key_type))
  }

  /** Pack a key into internal format. Parameters per WalletProposeRs */
  def packSigningKeyFromRFC1751(master_key: String, key_type: String): Either[AppError, SigningKey] = {
    RippleFormatConverters
    .convertMasterKey2masterSeedHex(master_key)
    .flatMap(packSigningKey(_, key_type))
  }
  /**
    * Mimics a SignRq as much as possible. The SignRs is not returned, instead
    * just the TxBlob for use in the SubmitRq
    * Note that the Fee should already be specified, also all the paths.
    *
    * @param tx_json TxJson with all default and autofillable fields (Fee/paths etc). Including SigningPubKey
    * @param signingKey Precomputed keypair for the actual signer (i.e. Account Key)    *
    * @return The signed TxBlob for inclusion in a SubmitRq
    */
  def sign(tx_json: JsonObject, signingKey: SigningKey): Either[AppError, TxnSignature] = {
    Signer.signToTxnSignature(tx_json, signingKey)
  }

  /**
    * Takes a signed tx_json object and verified the TxnSignature usign SigningPubKey
    * @param tx_json
    * @return true if verified correctly
    */
  def verify(tx_json: JsonObject): Either[AppError, Boolean] = {
    Verify.verifySigningResponse(tx_json)
  }

  /**
    * Expects a top level JsonObject representing a JSON document
    * that would be sent to rippled server. All isSerializable fields serialized.
    *
    * @param jsonObject
    *
    * @return Hex string representing the serialization in total.
    */
  def binarySerialize(jsonObject: JsonObject): Either[AppError, BinarySerializer.NestedEncodedValues] = {
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
  def binarySerializeForSigning(tx_json: JsonObject): Either[AppError, BinarySerializer.NestedEncodedValues] = {
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
