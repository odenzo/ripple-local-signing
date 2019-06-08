package com.odenzo.ripple.localops

import java.security.KeyPair

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe.{Json, JsonObject}

import com.odenzo.ripple.bincodec.serializing.BinarySerializer
import com.odenzo.ripple.bincodec.serializing.DebuggingShows._
import com.odenzo.ripple.localops.Verify.{edVerify, secpVerify}
import com.odenzo.ripple.localops.crypto.core.{ED25519CryptoBC, HashingOps, Secp256K1CryptoBC}
import com.odenzo.ripple.localops.crypto.AccountFamily
import com.odenzo.ripple.localops.reference.HashPrefix
import com.odenzo.ripple.localops.utils.caterrors.AppError
import com.odenzo.ripple.localops.utils.{ByteUtils, JsonUtils}

/** This takes a message and signs it. Returning the TxBlob
  *
  */
object Signer extends StrictLogging with JsonUtils with ByteUtils {

  /**
    *
    * @param tx_json     Filled tx_json, including SingingPubKey
    * @param master_seed Base58 master_seed Key
    *
    * @return TxnSignature
    */
  def sign(tx_json: JsonObject, master_seed: String, keyType: String): Either[AppError, String] = {

    // Basic Approach....
    // 1. Serialize txblob style
    // 2. Add the Transaction Prefix bytes
    // 3. Sha512Half hash
    // 4. Make Native KeyPair from Base58 secret
    // 5. Sign (assuming secp key for now)
    // 6. Add hex of Signature as TxnSignature
    // 7. Create a new TxBlob from updated tx_json
    // 8. Double check given SigningPubKey with generated PubKey from Secret
    // 9. Should also try to verify once I get that working
    val txnsignature: Either[AppError, String] = for {
      binBytes <- RippleLocalAPI.serializeForSigning(tx_json)
      payload  = HashPrefix.transactionSig.asBytes ++ binBytes // Inner Transaction! 0x53545800L

      ans ← keyType match {
             case "ed25519"   ⇒ signEd(master_seed, payload)
             case "secp256k1" ⇒ signSecp2(master_seed, payload)
             case other       ⇒ AppError(s"Unknown Key Type $other").asLeft

           }
    } yield ans
    txnsignature
  }

  def signEd(seedB58: String, payload: Array[Byte]): Either[AppError, String] = {
    val txnsig = for {
      kp     <- ED25519CryptoBC.seedHex2keypair(seedB58)
      sig    = ED25519CryptoBC.edSign(payload, kp)
      sigHex = bytes2hex(sig)
    } yield sigHex
    txnsig
  }

  /** Signs the message and returns the signaure in hex.
    *  Corrsponds to TxnSignature field
    * @param payload  The Binary Serialized for Signing bytes, with Signature Prefix
    * @param masterSeedB58
    * @return
    */
  def signSecp2(masterSeedB58: String, payload: Array[Byte]): Either[AppError, String] = {
    for {
      kp       <- AccountFamily.rebuildAccountKeyPairFromSeedB58(masterSeedB58)
      hashed   = HashingOps.sha256Ripple(payload)
      der <- Secp256K1CryptoBC.sign(hashed.toArray, kp)
    } yield der.toHex
  }

  /**
    *
    * @param tx_json     Filled tx_json, including SingingPubKey
    * @param txnSignature In Hex format
    *
    * @return Updated tx_blob in hex form for use in Submit call.
    */
  def createSignTxBlob(tx_json: JsonObject, txnSignature: String): Either[AppError, String] = {
    // Could add the HashPrefix. and get the hash if needed, e.g. to recreate SignRs message
    val withSig = addTxnSignature(tx_json, txnSignature)
    val txBlob = RippleLocalAPI.serialize(withSig)
    txBlob.map(v⇒ bytes2hex(v))
  }



  def addTxnSignature(txjson: JsonObject, txnSig: String): JsonObject = {
    txjson.add("TxnSignature", Json.fromString(txnSig))
  }


}
