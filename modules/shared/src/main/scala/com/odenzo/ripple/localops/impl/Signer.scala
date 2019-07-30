package com.odenzo.ripple.localops.impl

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import scribe.Logging

import com.odenzo.ripple.bincodec.RippleCodecAPI
import com.odenzo.ripple.localops._
import com.odenzo.ripple.localops.impl.crypto.AccountFamily
import com.odenzo.ripple.localops.impl.crypto.core.{ED25519CryptoBC, HashOps, Secp256K1CryptoBC}
import com.odenzo.ripple.localops.impl.reference.HashPrefix
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.models.{
  ED25519,
  KeyType,
  SECP256K1,
  SigningKey,
  SigningKeyEd25519,
  SigningKeySecp256,
  TxnSignature
}

/** This takes a message and signs it. Returning the TxBlob
  *
  */
object Signer extends Logging with BinCodecProxy with JsonUtils with ByteUtils {

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
      encoded <- binarySerializeForSigning(tx_json)
      binBytes = encoded.toBytes
      payload  = HashPrefix.transactionSig.asBytes ++ binBytes.toIndexedSeq // Inner Transaction
      ans <- key match {
        case k: SigningKeyEd25519 => signEd(k, payload)
        case k: SigningKeySecp256 => signSecp(k, payload)
      }
    } yield ans

  }

  /**
    *  This adds a signer field to the given tx_json, adding to any optional existing signers.
    * @param tx_json
    * @param key
    * @param signAddrB58Check
    * @return updated tx_json
    */
  def signFor(tx_json: JsonObject, key: SigningKey, signAddrB58Check: String): Either[Throwable, JsonObject] = {

    for {
      sig <- signForTxnSignature(tx_json, key, signAddrB58Check)
      txjson = createSuccessTxJson(tx_json, signAddrB58Check, sig, key.signPubKey)
    } yield txjson
  }

  /**
    * For sorting the Signer  by accounts within Signers array. Signer are fields in singleton object
    * Not sure we can sort on Base58 or need to convert to hex and sort pure numerically
    *
    * @param wrappedObject
    *
    * @return
    */
  def signerSortBy(wrappedObject: JsonObject): Option[String] = {
    for {
      signer  <- wrappedObject("Signer").flatMap(_.asObject)
      account <- signer("Account").flatMap(_.asString)
    } yield account

  }

  /**
    *
    * @param rqTxJson The Request TxJson -- this may or may not have Signers filled in.
    *
    * @return Response tx_json supplemented with single SignFor (no hash)
    */
  def createSuccessTxJson(rqTxJson: JsonObject, account: String, sig: TxnSignature, pubKey: String): JsonObject = {
    val signer: JsonObject = createSignerObject(account, sig, pubKey)
    val signers: List[JsonObject] =
      findField("Signers", rqTxJson).flatMap(json2arrayOfObjects).getOrElse(List.empty[JsonObject])

    // Each Signer fields should be sorted, and the order of Signer in the Signers array nees to be sorted.
    val updatedArray: List[JsonObject]  = signer :: signers
    val sortedSigners: List[JsonObject] = updatedArray.sortBy(Signer.signerSortBy)
    val rsTxJson: JsonObject            = rqTxJson.remove("Signers")
    val sortedTxJson                    = sortDeepFields(rsTxJson)
    val updatedSortedTxJson             = sortFields(sortedTxJson.add("Signers", sortedSigners.asJson))
    updatedSortedTxJson
  }

  def createSignerObject(account: String, sig: TxnSignature, pubKey: String): JsonObject = {
    JsonObject(
      "Signer" := JsonObject("Account" := account, "SigningPubKey" := pubKey, "TxnSignature" := sig.hex)
    )
  }

  /**
    *   This takes the Signers object, which is a list of zero or more Signer and merges them.
    *   Signer is a JsonArray of JsonObject, each object a Signer
    * @param signers  The values of the Signers field. Could be Json.Null I guess.
    *               @return Signers field value, a JsonArray of JsonObjects
    */
  def combineSignersObjects(signers: Json): Either[AppError, Json] = {
    json2arrayOfObjects(signers).map(combineSignerObjects)
  }

  /**
    *  TODO: Check the empty list case, now it returns [] which is ok I guess?
    * @param signers List of objects represent Signer (NOT SIGNERS now)
    *                @return A Signers object, with all the Singer fields enclosed and sorted.
    */
  def combineSignerObjects(signers: List[JsonObject]): Json = {
    val sortedSigners = signers.sortBy(signerSortBy)
    sortedSigners.asJson
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
        .leftMap(e => AppError(s"Serializing Address  $signerAddr", e))
      binBytes = encoded.toBytes
      payload  = HashPrefix.transactionMultiSig.asBytes ++ binBytes ++ address

      ans <- key match {
        case k: SigningKeyEd25519 => signEd(k, payload)
        case k: SigningKeySecp256 => signSecp(k, payload)
      }
    } yield ans

  }

  def signEd(keys: SigningKeyEd25519, payload: Seq[Byte]): Either[AppError, TxnSignature] = {
    for {
      sig <- ED25519CryptoBC.sign(payload, keys.kp)
      sigHex = bytes2hex(sig)
    } yield TxnSignature(sigHex)

  }

  def signSecp(keys: SigningKeySecp256, payload: Seq[Byte]): Either[AppError, TxnSignature] = {
    val hashed = HashOps.sha512Half(payload.toIndexedSeq)
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
  def createResponseHashHex(txblob: Seq[Byte]): String = {
    // return new Hash256(sha512Half(HashPrefix.transactionID, serialized));
    val payload: IndexedSeq[Byte]   = HashPrefix.transactionID.asBytes ++ txblob
    val hashBytes: IndexedSeq[Byte] = HashOps.sha512Half(payload)
    val hex                         = bytes2hex(hashBytes)
    hex
  }

}
