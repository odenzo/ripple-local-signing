package com.odenzo.ripple.localops.impl

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}

import com.odenzo.ripple.localops.impl.Sign.signForTxnSignature
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils, RippleBase58}
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.models.SigningKey

/**
  * Multisigning functions. signForSignerOnly does the actual work.
  * The rest of the routines are just for manipulating Json messages.
  **/
trait SignFor extends JsonUtils {

  /** Multisigns a tx_json returning the new Signer object.
    *  */
  def signForSignerOnly(tx_json: JsonObject, key: SigningKey, addr: String): Either[Throwable, JsonObject] = {
    signForTxnSignature(tx_json, key, addr).map { sig =>
      JsonObject(
        "Signer" := JsonObject("Account" := addr, "SigningPubKey" := key.signPubKey, "TxnSignature" := sig.hex)
      )
    }
  }

  /**
    *  This *adds* a Singer to the Signers field value array of the given tx_json
    *  If no Signers field exists an empty one is created.
    * @param tx_json
    * @param key
    * @param signAddrB58Check
    * @return updated tx_json  with new Signer, the hash is not updated
    */
  def signFor(tx_json: JsonObject, key: SigningKey, signAddrB58Check: String): Either[Throwable, JsonObject] = {
    for {
      signer <- signForSignerOnly(tx_json, key, signAddrB58Check)
      existingSigners = extractSignersFromTxJson(tx_json)
      updatedArray    = (signer :: existingSigners).sortBy(signerSortBy)
      rsTxJson        = JsonUtils.replaceField("Signers", tx_json, updatedArray.asJson)
    } yield sortFields(rsTxJson)
  }

  /**
    * For sorting the Signer  by accounts within Signers array. Signer are fields in singleton object
    * Not sure we can sort on Base58 or need to convert to hex and sort pure numerically
    * As quick hack we throw an exception instead of an Either since no Ordering defined on Either.
    * @param enclosingObj The object enclosing the Signer field whose value is also an object)
    *
    * @return
    */
  protected def signerSortBy(enclosingObj: JsonObject): String = {
    //FIXME: Hack so enclosing object containing Signer or the Signer obj value passed in
    val signer = findObjectField("Signer", enclosingObj).getOrElse(enclosingObj)
    val key = for {
      account <- findStringField("Account", signer)
      binary  <- RippleBase58.decode(account)
    } yield binary
    key match {
      case Right(v) => ByteUtils.bytes2hex(v.toIterable) // Scala 13 fix is not Scala 12 compatable
      case Left(err) =>
        val err = AppError("Signers Sort By Failed to Find Account", enclosingObj.asJson)
        logger.error(s"Hack Failed: Throwing ${err.show}")
        throw err
    }
  }

  /**
    *   Extraact Signer enclosing objects from result.
    *   It is mandatory at least one Signer enclosing object is present and must have Signer field
    *
    * @param signForRs
    */
  def extractSignersFromSignFor(signForRs: JsonObject): Either[AppError, List[JsonObject]] = {
    for {
      txjson <- findResultTxJson(signForRs)
      signers <- extractSignersFromTxJson(txjson) match {
        case Nil => AppError("No Signers fields found", signForRs.asJson).asLeft
        case ll =>
          if (ll.forall(_.contains("Signer"))) ll.asRight
          else AppError("Signers object had no Signer Field", signForRs.asJson).asLeft
      }
    } yield signers
  }

  /** Pulls the Signer field value from the tx_json.
    * @return If Signers field is not present or empty empty list is return
    * else it is an List of the Signer enclosing object (with Singer field inside) */
  def extractSignersFromTxJson(tx_json: JsonObject): List[JsonObject] = {
    // This is because this is applied to both request and response SignFor tx_json
    val signers: List[JsonObject] =
      findField("Signers", tx_json)
        .flatMap(json2arrayOfObjects)
        .getOrElse(List.empty[JsonObject])
    signers
  }

  /**
    *  Quick Lens like have,(TODO: make a real lens)
    *  Looks for result/tx_json and creates or replaces the existing Signers field with array of signers
    * @param newVal The array of Signers to set Singers field value to.
    * @param fullRs Full object to lens into at location ./result/tx_json/Signers
    * @return
    */
  def replaceSignersInFull(newVal: Json, fullRs: JsonObject): Either[Throwable, JsonObject] = {
    for {
      result <- findObjectField("result", fullRs)
      txjson <- findObjectField("tx_json", result)
      upTxjson = JsonUtils.replaceField("Signers", txjson, newVal)
      full <- replaceTxJsonInResult(fullRs, upTxjson.asJson)
    } yield full
  }

  def replaceTxJsonInResult(fullRs: JsonObject, updatedTxJson: Json): Either[Throwable, JsonObject] = {
    val jsonIn = fullRs.asJson
    jsonIn.hcursor
      .downField("result")
      .downField("tx_json")
      .set(updatedTxJson)
      .top
      .toRight(AppError("Trouble Replacing tx_json field", jsonIn))
      .flatMap(json2jsonObject)
  }

  /**
    *  Note this isn't technically correct as it just returns the first reponse with all the signers.
    *  The hash and TxnBlob etc are not updated, but the **typical** use case if just to pull out the signers for
    *  submit_multisigned?  So, why don't we just return that? or the Signers field value?
    *  So,
    * @param responses List of full response messages from sign_for commands
    * @return tx_json from first responses with aggregrated Signers field.
    */
  def mergeMultipleFullResponses(responses: List[JsonObject]): Either[Throwable, JsonObject] = {
    val txjsons = responses.traverse(findResultTxJson)
    txjsons.flatMap {
      case Nil         => AppError("Cannot combine empty list").asLeft
      case head :: Nil => head.asRight
      case fulllist @ head :: tail =>
        val signers: List[JsonObject] = fulllist.flatMap(extractSignersFromTxJson).distinct
        val sorted: List[JsonObject]  = signers.sortBy(signerSortBy)
        replaceSignersInFull(sorted.asJson, head)
    }

  }

}

object SignFor extends SignFor
