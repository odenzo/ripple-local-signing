package com.odenzo.ripple.localops.impl

import io.circe.Json

import cats._
import cats.data._
import cats.implicits._
import scribe.Logging

import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import com.odenzo.ripple.bincodec.{Decoded, EncodedSTObject, RippleCodecAPI, RippleCodecDebugAPI}
import com.odenzo.ripple.localops.LocalOpsError

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
  final def binarySerialize(jsonObject: Json): Either[LocalOpsError, EncodedSTObject] = {
    RippleCodecDebugAPI.binarySerialize(jsonObject).leftMap(LocalOpsError.wrapBinaryCodecError)
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
  final def binarySerializeForSigning(tx_json: Json): Either[LocalOpsError, EncodedSTObject] = {
    RippleCodecDebugAPI.binarySerializeForSigning(tx_json).leftMap(LocalOpsError.wrapBinaryCodecError)
  }

  /**
    * Serializes a transaction payload in Ripple binary format for constructing a TxnSignature from.
    *
    * @param tx_json
    *
    * @return Serialized form for signing, but without the HashPrefix.
    */
  final def serialize(tx_json: Json): Either[LocalOpsError, Array[Byte]] = {
    RippleCodecAPI.serializedTxBlob(tx_json).leftMap(LocalOpsError.wrapBinaryCodecError)
  }

  /**
    *
    * @param tx_json Fully formed tx_json with all auto-fillable fields etc.
    *
    * @return Byte Array representing the serialized for signing txn. Essentially TxBlob
    */
  final def serializeForSigning(tx_json: Json): Either[LocalOpsError, Array[Byte]] = {
    RippleCodecAPI.signingTxBlob(tx_json).leftMap(LocalOpsError.wrapBinaryCodecError)
  }

  /** Details here. Use case and function. Used for sorting Signer fields, does/should it strip prefix and suffix or
    * not */
  def serializeAddress(signAddrB58Check: String): Either[LocalOpsError, Array[Byte]] = {
    RippleCodecAPI.serializedAddress(signAddrB58Check).leftMap(LocalOpsError.wrapBinaryCodecError)
  }

  /** Debugging Routine mostly... */
  def decodeBlob(blob: String): Either[LocalOpsError, List[Decoded]] = {
    TxBlobBuster.bust(blob).leftMap(LocalOpsError.wrapBinaryCodecError)
  }
}

object BinCodecProxy extends BinCodecProxy
