package com.odenzo.ripple.localops.handlers

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import scribe.Logging

import com.odenzo.ripple.localops._
import com.odenzo.ripple.localops.crypto.RippleFormatConverters
import com.odenzo.ripple.localops.utils.caterrors.{AppError, OError}
import com.odenzo.ripple.localops.utils.{ByteUtils, JsonUtils}

/**
  *  The details of how this works are not documented, so I am assuming similar to Sign.
  * Basically, the tx_json has a empty ("") string for SigningPubKey.
  * The account address for the transaction has a set of "keys" set to its SignerList.
  *
  * SignFor adds one Signer to the array of Signers.  The Account and SignerPubKey are raw data.
  * The TxnSignature is calculated somehow.
  * I assume it is calculates the same way as a TxnSignature for normal signing case.
  * Testing with server shows that the TxnSignatures are invariant when the order of signing is changed.
  *
  * Remove heavy duplication between this and SignRqRsHandler was working.
  **/
object SignForRqRsHandler extends HandlerBase with Logging with JsonUtils with RippleFormatConverters {

  /**
    *
    * @param rq Full SignRq (SingingPubKey doesn't need to be filled)
    *
    * @return Left is error response SignRs, Right is success response
    */
  def signFor(rq: JsonObject): Either[JsonObject, JsonObject] = {

    val ok: Either[ResponseError, JsonObject] = for {
      _           ← validateRequest(rq)
      key         <- extractKey(rq) // One of the multisigners.
      signingAcct ← findStringField("account", rq).leftMap(err ⇒ ResponseError.invalid("Missing account field"))
      tx_json     ← JsonUtils.findObjectField("tx_json", rq).leftMap(err ⇒ ResponseError.kNoTxJson)
      sig         <- Signer.signForToTxnSignature(tx_json, key, signingAcct).leftMap(err ⇒ ResponseError.kBadSecret)
      txBlob      ← Signer.createSignedTxBlob(tx_json, sig).leftMap(err ⇒ ResponseError.kBadSecret)
      blobhex = ByteUtils.bytes2hex(txBlob)
      hash    = Signer.createResponseHash(txBlob)

      success = buildSuccessResponse(rq, tx_json, signingAcct, sig, blobhex, hash, key.signPubKey)
    } yield success

    ok.leftMap(re ⇒ buildFailureResponse(rq, re))

  }

  def validateAutofillFields(tx_json: JsonObject): Either[OError, JsonObject] = {
    List("Sequence", "Fee")
    if (tx_json.contains("Sequence") && tx_json.contains("Fee")) tx_json.asRight
    else AppError(s"Sequence and Fee must be present in tx_json").asLeft
  }

  /** Should requre Sequence and Fee too */
  def validateRequest(rq: JsonObject): Either[ResponseError, String] = {
    JsonUtils
      .findStringField("command", rq)
      .leftMap(_ ⇒ ResponseError.kNoCommand)
      .flatMap {
        case "sign_for" ⇒ "sign_for".asRight
        case other      ⇒ ResponseError.kBadCommand.asLeft
      }
  }

  /**
    * For sorting the Signer  by accounts within Signers array. Signer are fields in singleton object
    * Not sure we can sort on Base58 or need to convert to hex and sort pure numerically
    *
    * @param wrappedObject
    *
    * @return
    */
  def signerSortBy(wrappedObject: Json): Option[String] = {
    for {
      obj     ← wrappedObject.asObject
      signer  ← obj("Signer").flatMap(_.asObject)
      account ← signer("Account").flatMap(_.asString)
    } yield account

  }

  def buildSuccessResponse(rq: JsonObject,
                           rqTxJson: JsonObject,
                           account: String,
                           sig: TxnSignature,
                           signedBlob: String,
                           hash: String,
                           signPubKey: String): JsonObject = {

    val signers: Vector[Json] = rqTxJson("Signers").flatMap(_.asArray).getOrElse(Vector.empty[Json])

    val signer: JsonObject = JsonObject(
      "Signer" := JsonObject("Account" := account, "SigningPubKey" := signPubKey, "TxnSignature" := sig.hex)
    )

    val updatedArray: Vector[Json] = signer.asJson +: signers
    val sortedSigners              = updatedArray.sortBy(signerSortBy)

    val rsTxJson: JsonObject = rqTxJson.remove("Signers").add("hash", hash.asJson)
    val sortedTxJson         = sortDeepFields(rsTxJson)
    val updatedSortedTxJson  = sortFields(sortedTxJson.add("Signers", sortedSigners.asJson))
    val result               = JsonObject("tx_blob" := signedBlob, "tx_json" := updatedSortedTxJson)

    JsonObject
      .singleton("id", rq("id").getOrElse(Json.Null))
      .add("result", result.asJson)
      .add("status", Json.fromString("success"))
      .add("type", Json.fromString("response"))
  }

}
