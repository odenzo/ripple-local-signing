package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe._

import com.odenzo.ripple.localops.crypto.DERSignature
import com.odenzo.ripple.localops.crypto.core.{ED25519CryptoBC, HashOps, Secp256K1CryptoBC}
import com.odenzo.ripple.localops.reference.HashPrefix
import com.odenzo.ripple.localops.utils.caterrors.AppError
import com.odenzo.ripple.localops.utils.{ByteUtils, JsonUtils}

/**
  * Tries to verify a signed ripple transaction, with either Secp256k or ed25519 signatures.
  **/
object Verify extends StrictLogging with JsonUtils with ByteUtils {

  def verifySigningResponse(txjson: JsonObject): Either[AppError, Boolean] = {

    val verified = for {
      // Well, tx blob with appended prefix with or without TxnSig should equal soeting
      pubkeyraw  <- findField("SigningPubKey", txjson).flatMap(json2string)
      keyType    = if (pubkeyraw.startsWith("ED")) "ed25519" else "secp256k1"
      signature  <- findField("TxnSignature", txjson).flatMap(json2string)
      serialized <- RippleLocalAPI.serializeForSigning(txjson)
      payload    = HashPrefix.transactionSig.asBytes ++ serialized
      ans ← keyType match {
             case "ed25519"   ⇒ edVerify(signature, pubkeyraw, payload)
             case "secp256k1" ⇒ secpVerify(signature, pubkeyraw, payload)
             case other       ⇒ AppError(s"Unknown Key Type $other").asLeft

           }
    } yield ans
      verified
  }

  /**
    *
    * Verifis an ed25519 signed transaction values.
    *
    * @param signature Raw Hex Signature with the ED prefix, which will be dropped
    * @param pubkeyraw
    * @param payload
    *
    * @return
    */
  def edVerify(signature: String, pubkeyraw: String, payload: Seq[Byte]): Either[AppError, Boolean] = {
    for {
      pubkey   <- ED25519CryptoBC.signingPubKey2KeyParameter(pubkeyraw)
      sig      <- ByteUtils.hex2Bytes(signature)
      verified <- ED25519CryptoBC.edVerify(payload.toArray, sig.toArray, pubkey)
    } yield verified

  }

  def secpVerify(signature: String, pubkeyraw: String, payload: Seq[Byte]): Either[AppError, Boolean] = {
    val hash = HashOps.sha512Half(payload)
    for {
      pubkey <- hex2Bytes(pubkeyraw).map(v => Secp256K1CryptoBC.decompressPublicKey(v.toArray))
      sig    <- DERSignature.fromHex(signature)
      valid  <- Secp256K1CryptoBC.verify(hash.toArray, sig, pubkey)
    } yield valid
  }

}
