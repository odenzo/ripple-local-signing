package com.odenzo.ripple.localops.impl

import io.circe.optics.JsonPath
import io.circe.syntax._
import io.circe.{Json, JsonObject}

import cats._
import cats.data._
import cats.implicits._
import monocle.{Optional, Traversal}

import com.odenzo.ripple.bincodec.RippleCodecAPI
import com.odenzo.ripple.bincodec.utils.RippleBase58
import com.odenzo.ripple.localops.impl.Sign.binarySerializeForSigning
import com.odenzo.ripple.localops.impl.reference.HashPrefix
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.models.{SigningKey, TxnSignature}
import com.odenzo.ripple.localops.{LOpException, LocalOpsError, WrappedBinCodecErr}

/**
  * Multisigning functions. signForSignerOnly does the actual work.
  * The rest of the routines are just for manipulating Json messages.
  * Needs a real cleanup
  **/
trait SignFor extends JsonUtils {

  val txjsonGetLens: Traversal[Json, JsonObject] = JsonPath.root.Signers.each.obj

  // Want to work if not present but we
  val txjsonSetLens: Optional[Json, Vector[Json]] = JsonPath.root.Signers.arr
  val txjsonPubKeyAdd: Json => Json = JsonPath.root
    .at("SigningPubKey")
    .modify(opt => Json.fromString("").some)

  val replyGetLens: Traversal[Json, JsonObject]  = JsonPath.root.result.tx_json.Signers.each.obj
  val replySetLens: Optional[Json, Vector[Json]] = JsonPath.root.result.tx_json.Signers.arr

  /**
    *  This *adds* a Singer to the Signers field value array of the given tx_json
    *  If no Signers field exists an empty one is created.
    * @param tx_json
    * @param key
    * @param signAddrB58Check
    * @return updated tx_json  with new Signer, the hash is not updated
    */
  def signFor(tx_json: Json, key: SigningKey, signAddrB58Check: String): Either[Throwable, Json] = {
    val root = txjsonPubKeyAdd(tx_json)
    for {

      signer <- signForSignerOnly(root, key, signAddrB58Check)
      existingSigners = txjsonGetLens.getAll(root).map(_.asJson)
      updatedArray    = (signer :: existingSigners).distinct
      sorted <- sortSigners(updatedArray)
      hacked   = ensureSignersField(root)
      rsTxJson = JsonPath.root.Signers.arr.set(sorted.toVector)(hacked)
      reply <- json2object(rsTxJson).map(sortFields)
    } yield reply.asJson
  }

  protected def ensureSignersField(json: Json): Json = {
    json.mapObject { obj =>
      if (obj.contains("Signers")) obj
      else obj.add("Signers", List.empty[Json].asJson)
    }
  }

  /** Multisigns a tx_json returning the new Signer object.
    *  */
  protected def signForSignerOnly(tx_json: Json, key: SigningKey, addr: String): Either[Throwable, Json] = {
    // This will add the account address at the end. Not sure why I split across files.
    signForTxnSignature(tx_json, key, addr).map { sig =>
      JsonObject(
        "Signer" := JsonObject("Account" := addr, "SigningPubKey" := key.signPubKey, "TxnSignature" := sig.hex)
      ).asJson
    }
  }

  /**
    * Hmmm...  This is for the internal Signer TxnSignature
    * IT hases multisign hash prefix and appends the signer account before signing
    * @param tx_json of the requested txn, optionally with Existing Signers
    * @return TxnSignature which includes the the Signer account
    * */
  def signForTxnSignature(tx_json: Json, key: SigningKey, signerAddr: String): Either[Throwable, TxnSignature] = {

    // Well, first, we need to use different hash prefix. (transactionMultiSig)
    // Then a suffix is encoding of the signingAccount as bytes.
    // Also should make sure SigningPubKey="" is in tx_Json

    for {
      encoded <- binarySerializeForSigning(tx_json).leftMap(e => LocalOpsError("Error Serializing", e))
      address <- RippleCodecAPI
        .serializedAddress(signerAddr)
        .leftMap(e => LocalOpsError(s"Serializing Addr  $signerAddr", e))
      binBytes = encoded.toBytes
      payload  = HashPrefix.transactionMultiSig.asByteArray ++ binBytes ++ address
      ans <- Sign.signPayload(payload, key)
    } yield ans

  }

  /** Generates the TxBlob for fully multi-signed tx_json */
  def generateTxBlob(tx_json: Json): Either[LOpException, String] = {
    for {
      encoded <- BinCodecProxy.binarySerialize(tx_json).leftMap(e => LocalOpsError("Error Serializing", e))
      binBytes = encoded.toHex
    } yield binBytes
  }

  def sortSigners(encObj: List[Json]): Either[LocalOpsError, List[Json]] = {
    LocalOpsError.handle("Sorting Signers") {
      encObj.sortBy(v => unsafeSortFn(v))
    }
  }

  /**
    * For sorting the Signer  by accounts within Signers array. Signer are fields in singleton object
    * Not sure we can sort on Base58 or need to convert to hex and sort pure numerically
    * As quick hack we throw an exception instead of an Either since no Ordering defined on Either.
    * @param enclosingObj The object enclosing the Signer field whose value is also an object)
    *
    * @return
    */
  protected def signerSortBy(enclosingObj: Json): Either[LocalOpsError, String] = {
    // Want to sort by the decoded account in each signer
    // Our "cursor" is the top object in Signers array (anonymous json object)

    // I wonder how long it takes to "build" the Optional before applying?
    logger.debug(s"Sorting By With Root ${enclosingObj.spaces4}")
    val path: Optional[Json, String] = JsonPath.root.Signer.Account.string
    for {
      acct   <- lensGetOpt(path)(enclosingObj)
      binary <- RippleBase58.decode(acct).leftMap(WrappedBinCodecErr(_))
    } yield ByteUtils.bytes2hex(binary)
  }

  protected def unsafeSortFn(j: Json): String = {
    signerSortBy(j) match {
      case Left(err) => throw err
      case Right(v)  => v
    }
  }

  /**
    *  Note this isn't technically correct as it just returns the first reponse with all the signers.
    *  The hash and TxnBlob etc are not updated, but the **typical** use case if just to pull out the signers for
    *  submit_multisigned?  So, why don't we just return that? or the Signers field value?
    *  So,
    * @param responses List of full response messages from sign_for commands
    * @return tx_json from first responses with aggregrated Signers field.
    */
  def mergeMultipleFullResponses(responses: List[Json]): Either[LocalOpsError, Json] = {

    responses match {
      case Nil                     => LocalOpsError("Cannot combine empty list").asLeft
      case head :: Nil             => head.asRight
      case fulllist @ head :: tail =>
        // Each response may have multiple Signer in general case
        val signers: List[Json] = fulllist.flatMap(rs => replyGetLens.getAll(rs.asJson)).distinct.map(_.asJson)
        sortSigners(signers).map(sl => replySetLens.set(sl.toVector)(head.asJson))
    }
  }

  /** Accepts tx_json instead of full responses
    * TODO: This is obtuse, switch to optics or re-write somehow.
    * @return  The first tx_json with updated Signers
    */
  def mergeMultipleTxJsonResponses(txjsonRs: List[Json]): Either[LocalOpsError, Json] = {
    txjsonRs match {
      case Nil                     => LocalOpsError("Cannot combine empty list").asLeft
      case head :: Nil             => head.asRight
      case fulllist @ head :: tail =>
        // Each response may have multiple Signer in general case
        val signers: List[Json] = fulllist.flatMap(rs => replyGetLens.getAll(rs.asJson)).distinct.map(_.asJson)
        sortSigners(signers).map(sl => replySetLens.set(sl.toVector)(head.asJson))
    }
  }

}

object SignFor extends SignFor
