package com.odenzo.ripple.localops.impl

import cats._
import cats.data._
import cats.implicits._
import io.circe._
import scribe.Logging

import com.odenzo.ripple.localops.BinCodecProxy
import com.odenzo.ripple.localops.impl.crypto.DERSignature
import com.odenzo.ripple.localops.impl.crypto.core.{ED25519CryptoBC, HashOps, Secp256K1CryptoBC}
import com.odenzo.ripple.localops.impl.reference.HashPrefix
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}

/**
  * Tries to verify a signed ripple transaction, with either Secp256k or ed25519 signatures.
  **/
trait Verify extends Logging with JsonUtils with ByteUtils {

  def verifySigningResponse(txjson: JsonObject): Either[AppError, Boolean] = {

    val verified = for {
      // Well, tx blob with appended prefix with or without TxnSig should equal soeting
      pubkeyraw <- findField("SigningPubKey", txjson).flatMap(json2string)
      keyType = if (pubkeyraw.startsWith("ED")) "ed25519" else "secp256k1"
      signature  <- findField("TxnSignature", txjson).flatMap(json2string)
      serialized <- BinCodecProxy.serializeForSigning(txjson)
      payload = HashPrefix.transactionSig.asBytes ++ serialized
      ans ← keyType match {
        case "ed25519"   ⇒ ed25519(signature, pubkeyraw, payload)
        case "secp256k1" ⇒ secp256k1(signature, pubkeyraw, payload)
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
  protected def ed25519(signature: String, pubkeyraw: String, payload: Seq[Byte]): Either[AppError, Boolean] = {
    for {
      pubkey   <- ED25519CryptoBC.signingPubKey2KeyParameter(pubkeyraw)
      sig      <- ByteUtils.hex2bytes(signature)
      verified <- ED25519CryptoBC.verify(payload.toArray, sig.toArray, pubkey)
    } yield verified

  }

  protected def secp256k1(signature: String, pubkeyraw: String, payload: Seq[Byte]): Either[AppError, Boolean] = {
    val hash = HashOps.sha512Half(payload)
    for {
      pubkey <- hex2bytes(pubkeyraw).map(v => Secp256K1CryptoBC.decompressPublicKey(v.toArray))
      sig    <- DERSignature.fromHex(signature)
      valid  <- Secp256K1CryptoBC.verify(hash.toArray, sig, pubkey)
    } yield valid
  }

}

object Verify extends Verify