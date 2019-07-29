package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import scribe.Logging

import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError
import com.odenzo.ripple.bincodec.{Decoded, EncodedSTObject, RippleCodecAPI}
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError

/** Production level proxying to ripple-binary-codec routines just to add a layer and error conversion.
  * Error conversion done because we want ripple-binary-codec to be usable without any binding into this lib.
  * This whole thing could be an implementation details, but maybe some use exposing it?
  * */
trait BinCodecProxy extends Logging {

  /**
    * Serializes a transaction payload in Ripple binary format for constructing a TxBlob from.
    * All isSerializable fields serialized.
    *
    * @param jsonObject tx_json with PubSigningKey and TxnSignature added
    *
    * @return Hex string representing the serialization in total.
    */
  @inline
  final def binarySerialize(jsonObject: JsonObject): Either[AppError, EncodedSTObject] = {
    RippleCodecAPI.binarySerialize(jsonObject).leftMap(AppError.wrapCodecError)
  }

  /**
    * Serializes a transaction payload in Ripple binary format for constructing a TxnSignature from.
    *
    * @param tx_json JsonObject representing a transaction tx_json, no auto-filling of fields is done.
    *
    * @return Encoded/Serialized structure that can be inspected or converted to bytes.
    *
    */
  @inline
  final def binarySerializeForSigning(tx_json: JsonObject): Either[AppError, EncodedSTObject] = {
    RippleCodecAPI.binarySerializeForSigning(tx_json).leftMap(AppError.wrapCodecError)
  }

  /**
    * Serializes a transaction payload in Ripple binary format for constructing a TxnSignature from.
    *
    * @param tx_json
    *
    * @return Serialized form for signing, but without the HashPrefix.
    */
  @inline
  final def serialize(tx_json: JsonObject): Either[AppError, Array[Byte]] = {
    RippleCodecAPI.serializedTxBlob(tx_json).leftMap(AppError.wrapCodecError)
  }

  /**
    *
    * @param tx_json Fully formed tx_json with all auto-fillable fields etc.
    *
    * @return Byte Array representing the serialized for signing txn. Essentially TxBlob
    */
  @inline
  final def serializeForSigning(tx_json: JsonObject): Either[AppError, Array[Byte]] = {
    RippleCodecAPI.signingTxBlob(tx_json).leftMap(AppError.wrapCodecError)
  }

  /** Details here. Use case and function. Used for sorting Signer fields, does/should it strip prefix and suffix or
    * not */
  def serializeAddress(signAddrB58Check: String): Either[RippleCodecError, Array[Byte]] = {
    RippleCodecAPI.serializedAddress(signAddrB58Check)
  }

  /** Debugging Routine mostly... */
  def decodeBlob(blob: String): Either[AppError, List[Decoded]] = {
    TxBlobBuster.bust(blob).leftMap(AppError.wrapCodecError)
  }
}

object BinCodecProxy extends BinCodecProxy
