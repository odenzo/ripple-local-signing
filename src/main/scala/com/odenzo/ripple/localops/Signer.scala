package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import scribe.Logging
import io.circe.{Json, JsonObject}
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

import com.odenzo.ripple.localops.crypto.AccountFamily
import com.odenzo.ripple.localops.crypto.core.{ED25519CryptoBC, HashOps, Secp256K1CryptoBC}
import com.odenzo.ripple.localops.reference.HashPrefix
import com.odenzo.ripple.localops.utils.caterrors.AppError
import com.odenzo.ripple.localops.utils.{ByteUtils, JsonUtils}

/** This takes a message and signs it. Returning the TxBlob
  *
  */
object Signer extends Logging with JsonUtils with ByteUtils {

  def preCalcKeys(seedhex: String, keyType: String): Either[AppError, SigningKey] = {
    keyType match {
      case "ed25519" ⇒
        for {
          keys <- ED25519CryptoBC.generateKeyPairFromHex(seedhex)
          spub ← ED25519CryptoBC.publicKey2Hex(keys.getPublic)
        } yield SigningKeyEd25519(keys, spub)
      case "secp256k1" ⇒
        for {
          keys <- AccountFamily.rebuildAccountKeyPairFromSeedHex(seedhex)
          spub = Secp256K1CryptoBC.publicKey2hex(keys.getPublic)
        } yield SigningKeySecp256(keys, spub)

      case other ⇒ AppError(s"Unsupported key type $keyType -- ed25519 or sec256k1").asLeft
    }
  }

  /**
    *
    * @param tx_json     Filled tx_json, including SingingPubKey
    *
    * @return TxnSignature
    */
  def signToTxnSignature(tx_json: JsonObject, key: SigningKey): Either[AppError, TxnSignature] = {

    for {
      encoded <- RippleLocalAPI.binarySerializeForSigning(tx_json)
      binBytes = encoded.toBytes
      payload  = HashPrefix.transactionSig.asBytes ++ binBytes // Inner Transaction


      ans <- key match {
              case k: SigningKeyEd25519 ⇒ signEd(k, payload)
              case k: SigningKeySecp256 ⇒ signSecp(k, payload)
            }
    } yield ans

  }

  def signEd(keys: SigningKeyEd25519, payload: Array[Byte]): Either[AppError, TxnSignature] = {
    for {
      sig    <- ED25519CryptoBC.sign(payload, keys.kp)
      sigHex = bytes2hex(sig)
    } yield TxnSignature(sigHex)

  }

  def signSecp(keys: SigningKeySecp256, payload: Array[Byte]): Either[AppError, TxnSignature] = {
    val hashed = HashOps.sha512Half(payload)
    Secp256K1CryptoBC.sign(hashed.toArray, keys.kp).map(b ⇒ TxnSignature(b.toHex))
  }

  /**
    *
    * @param tx_json     Filled tx_json, including SingingPubKey
    * @param txnSignature In Hex format
    *
    * @return Updated tx_blob in hex form for use in Submit call.
    */
  def createSignedTxBlob(tx_json: JsonObject, txnSignature: TxnSignature): Either[AppError, Array[Byte]] = {
    // Could add the HashPrefix. and get the hash if needed, e.g. to recreate SignRs message
    val withSig = tx_json.add("TxnSignature", Json.fromString(txnSignature.hex))
    RippleLocalAPI.serialize(withSig)
  }

  def createResponseHash(txblob: Array[Byte]): String = {
    bytes2hex(HashOps.sha512(txblob))
  }

}
