package com.odenzo.ripple.localops.impl

import cats.implicits._
import io.circe.{Json, JsonObject}
import scribe.Logging

import com.odenzo.ripple.bincodec.RippleCodecAPI
import com.odenzo.ripple.localops.impl.crypto.AccountFamily
import com.odenzo.ripple.localops.impl.crypto.core.{ED25519CryptoBC, HashOps, Secp256K1CryptoBC}
import com.odenzo.ripple.localops.impl.reference.HashPrefix
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.models._

/** This takes a message and signs it. Returning the TxBlob
  *
  */
object Sign extends Logging with BinCodecProxy with JsonUtils with ByteUtils {

  /** ToDo: Nice to put Address in this for use in MultiSigning future APIs */
  def preCalcKeys(seedhex: String, keyType: KeyType): Either[AppError, SigningKey] = {
    keyType match {
      case ED25519 =>
        for {
          keys <- ED25519CryptoBC.generateKeyPairFromHex(seedhex)
          spub <- ED25519CryptoBC.publicKey2Hex(keys.getPublic)
        } yield SigningKeyEd25519(keys, spub)
      case SECP256K1 =>
        for {
          keys <- AccountFamily.rebuildAccountKeyPairFromSeedHex(seedhex)
          spub = Secp256K1CryptoBC.publicKey2hex(keys.getPublic)
        } yield SigningKeySecp256(keys, spub)
    }
  }

  /**
    * This does the binary serialize for signing (only isSigning fields), adds TransactionSig Prefix then signs.
    *
    * @param tx_json Filled tx_json, including SingingPubKey
    *
    * @return TxnSignature
    */
  def signToTxnSignature(tx_json: JsonObject, key: SigningKey): Either[AppError, TxnSignature] = {

    for {
      encoded <- BinCodecProxy.binarySerializeForSigning(tx_json)
      binBytes = encoded.toBytes
      payload  = HashPrefix.transactionSig.asByteArray ++ binBytes
      ans <- key match {
        case k: SigningKeyEd25519 => signEd(k, payload)
        case k: SigningKeySecp256 => signSecp(k, payload)
      }
    } yield ans

  }

  /** Internal API
    *
    * @param tx_json of the requested txn, optionally with Existing Signers
    * @return TxnSignature which includes the the Signer account
    * */
  def signForTxnSignature(tx_json: JsonObject, key: SigningKey, signerAddr: String): Either[Throwable, TxnSignature] = {

    // Well, first, we need to use different hash prefix. (transactionMultiSig)
    // Then a suffix is encoding of the signingAccount as bytes.
    //
    for {
      encoded <- binarySerializeForSigning(tx_json).leftMap(e => AppError("Error Serializing", e))
      address <- RippleCodecAPI
        .serializedAddress(signerAddr)
        .leftMap(e => AppError(s"Serializing Addr  $signerAddr", e))
      binBytes = encoded.toBytes
      payload  = HashPrefix.transactionMultiSig.asByteArray ++ binBytes ++ address

      ans <- key match {
        case k: SigningKeyEd25519 => signEd(k, payload)
        case k: SigningKeySecp256 => signSecp(k, payload)
      }
    } yield ans

  }

  def signEd(keys: SigningKeyEd25519, payload: Array[Byte]): Either[AppError, TxnSignature] = {
    for {
      sig <- ED25519CryptoBC.sign(payload, keys.kp)
      sigHex = bytes2hex(sig)
    } yield TxnSignature(sigHex)

  }

  def signSecp(keys: SigningKeySecp256, payload: Array[Byte]): Either[AppError, TxnSignature] = {
    val hashed = HashOps.sha512Half(payload)
    Secp256K1CryptoBC.sign(hashed.toArray, keys.kp).map(b => TxnSignature(b.toHex))
  }

  /**
    *
    * @param tx_json      Filled tx_json, including SingingPubKey
    * @param txnSignature In Hex format, Empty String when multisigning.
    *
    * @return Updated tx_blob in hex form for use in Submit call.
    */
  def createSignedTxBlob(tx_json: JsonObject, txnSignature: TxnSignature): Either[AppError, Array[Byte]] = {
    // Could add the HashPrefix. and get the hash if needed, e.g. to recreate SignRs message
    val withSig = tx_json.add("TxnSignature", Json.fromString(txnSignature.hex))
    BinCodecProxy.serialize(withSig)
  }

  /**
    * Has this been thoroughly tested?
    * Should be 32 bytes
    * TODO: Broken! And not always used as part of calc hash logic
    *
    * @param txblob Is this a SigningTxBlob or all TxBlob
    *
    * @return Calculates a response objects Hash (in hex) from tx_blob
    */
  def createResponseHashHex(txblob: Array[Byte]): String = {
    // return new Hash256(sha512Half(HashPrefix.transactionID, serialized));
    val payload: Seq[Byte]          = HashPrefix.transactionID.asBytes ++ txblob
    val hashBytes: IndexedSeq[Byte] = HashOps.sha512Half(payload.toArray)
    val hex                         = bytes2hex(hashBytes)
    hex
  }

}
