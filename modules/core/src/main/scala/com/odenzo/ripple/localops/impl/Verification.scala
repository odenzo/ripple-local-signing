package com.odenzo.ripple.localops.impl

import io.circe._
import io.circe.optics.JsonPath

import cats._
import cats.data._
import cats.implicits._
import scribe.Logging

import com.odenzo.ripple.localops.LocalOpsError
import com.odenzo.ripple.localops.impl.crypto.DERSignature
import com.odenzo.ripple.localops.impl.crypto.core.{ED25519CryptoBC, HashOps, Secp256K1CryptoBC}
import com.odenzo.ripple.localops.impl.reference.HashPrefix
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}

/**
  * Tries to verify a signed ripple transaction, with either Secp256k or ed25519 signatures.
  * TODO: Add support for multisignings
  **/
trait Verification extends Logging with JsonUtils with ByteUtils {

  def verifySignedResponse(txjson: Json): Either[LocalOpsError, Boolean] = {

    if (hasField("TxnSignature", txjson)) verifySingleSign(txjson)
    else if (hasField("Signers", txjson)) verifyMultiSigned(txjson)
    else LocalOpsError("No SigningPubKey or Signers found to verify").asLeft
  }

  def verifySingleSign(txjson: Json): Either[LocalOpsError, Boolean] = {
    logger.debug("Verifying Single Signing")
    val verified = for {
      serialized <- BinCodecProxy.binarySerializeForSigning(txjson)
      payload = HashPrefix.transactionSig.asByteArray ++ serialized.toBytes

      pubkeyraw <- findField("SigningPubKey", txjson).flatMap(json2string)
      signature <- findField("TxnSignature", txjson).flatMap(json2string)

      ans <- verifyOneSigner(pubkeyraw, signature, payload.toIndexedSeq)
    } yield ans
    verified
  }

  /** Verified all signatures, w/o cross checking public key matches account. In Progress */
  def verifyMultiSigned(txjson: Json): Either[LocalOpsError, Boolean] = {
    logger.warn("Attempting MultiSig Verification - In Progress")
    val prepared = txjson //.remove("TxnSignature").remove("SigningPubKey")
    for {
      serialized <- BinCodecProxy.binarySerializeForSigning(txjson)
      signers = JsonPath.root.Signers.each.Signer.json.getAll(txjson) // actually obj

      each <- signers.traverse { signer =>
        for {
          account   <- findFieldAsString("Account", signer)
          rawPubKey <- findFieldAsString("SigningPubKey", signer)
          signature <- findFieldAsString("TxnSignature", signer)

          accountSuffix <- BinCodecProxy.serializeAddress(account)
          payload = HashPrefix.transactionMultiSig.asBytes ++ serialized.toBytes ++ accountSuffix
          ans <- verifyOneSigner(rawPubKey, signature, payload)
        } yield ans
      }
      _   = logger.info(s"Per Signer: ${each}")
      all = each.forall(_ === true)
    } yield all
  }

  protected def verifyOneSigner(
      rawPubKey: String,
      rawSignature: String,
      payload: IndexedSeq[Byte]
  ): Either[LocalOpsError, Boolean] = {

    logger.debug(s"Verifying One Signer with PubKey: $rawPubKey  Sig: $rawSignature")
    val keyType = if (rawPubKey.startsWith("ED")) "ed25519" else "secp256k1"
    keyType match {
      case "ed25519"   => ed25519(rawSignature, rawPubKey, payload)
      case "secp256k1" => secp256k1(rawSignature, rawPubKey, payload)
      case other       => LocalOpsError(s"Unknown Key Type $other").asLeft

    }
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
  protected def ed25519(
      signature: String,
      pubkeyraw: String,
      payload: IndexedSeq[Byte]
  ): Either[LocalOpsError, Boolean] = {
    for {
      pubkey   <- ED25519CryptoBC.signingPubKey2KeyParameter(pubkeyraw)
      sig      <- ByteUtils.hex2bytes(signature)
      verified <- ED25519CryptoBC.verify(payload.toArray, sig.toArray, pubkey)
    } yield verified

  }

  protected def secp256k1(
      signature: String,
      pubkeyraw: String,
      payload: IndexedSeq[Byte]
  ): Either[LocalOpsError, Boolean] = {

    for {
      pubkey <- hex2bytes(pubkeyraw).map(v => Secp256K1CryptoBC.decompressPublicKey(v.toArray))
      sig    <- DERSignature.fromHex(signature)
      hash = HashOps.sha512Half(payload.toArray)
      valid <- Secp256K1CryptoBC.verify(hash.toArray, sig, pubkey)
    } yield valid
  }

}

object Verification extends Verification
