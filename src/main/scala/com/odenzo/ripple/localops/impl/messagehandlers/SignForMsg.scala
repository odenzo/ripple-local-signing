package com.odenzo.ripple.localops.impl.messagehandlers

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import scribe.Logging

import com.odenzo.ripple.localops.impl.crypto.RippleFormatConverters
import com.odenzo.ripple.localops.impl.utils.caterrors.{AppError, OError}
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.impl.{BinCodecProxy, Sign, SignFor}
import com.odenzo.ripple.localops.models.ResponseError

/**
  * The details of how this works are not documented, so I am assuming similar to Sign.
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
object SignForMsg extends HandlerBase with Logging with JsonUtils with RippleFormatConverters {

  /**
    *
    * @param rq Full SignRq (SingingPubKey doesn't need to be filled)
    *
    * @return Left is error response SignRs, Right is success response
    */
  def signFor(rq: Json): Either[JsonObject, JsonObject] = {
    logger.debug(s"signFor Rq: \n ${rq.spaces4}")
    val ok: Either[ResponseError, JsonObject] = for {
      jobj        <- json2jsonObject(rq).leftMap(err => ResponseError.invalid("Rq Not JsonObject"))
      _           <- validateRequest(jobj)
      key         <- extractKey(jobj) // One of the multisigners.
      signingAcct <- findStringField("account", jobj).leftMap(err => ResponseError.invalid("Missing account field"))
      tx_json     <- JsonUtils.findObjectField("tx_json", jobj).leftMap(err => ResponseError.kNoTxJson)
      tx_jsonOut  <- SignFor.signFor(tx_json, key, signingAcct).leftMap(err => ResponseError.kBadSecret)
      txBlob      <- BinCodecProxy.serialize(tx_jsonOut).leftMap(err => ResponseError.invalid(s"Internal Error ${err.msg}"))
      blobhex = ByteUtils.bytes2hex(txBlob)
      hash    = Sign.createResponseHashHex(txBlob)
      success = buildSuccessResponse(jobj("id"), tx_jsonOut, blobhex, hash)
    } yield success

    ok.leftMap(re => buildFailureResponse(rq, re))

  }

  def validateAutofillFields(tx_json: JsonObject): Either[OError, JsonObject] = {
    List("Sequence", "Fee").forall(tx_json.contains) match {
      case true  => tx_json.asRight
      case false => AppError(s"Sequence and Fee must be present in tx_json").asLeft
    }
  }

  /** Should requre Sequence and Fee too */
  def validateRequest(rq: JsonObject): Either[ResponseError, String] = {
    JsonUtils
      .findStringField("command", rq)
      .leftMap(_ => ResponseError.kNoCommand)
      .flatMap {
        case "sign_for" => "sign_for".asRight
        case other      => ResponseError.kBadCommand.asLeft
      }
  }

  def buildSuccessResponse(id: Option[Json], txJson: JsonObject, txBlob: String, hash: String): JsonObject = {
    val sortedTx = sortDeepFields(txJson.add("hash", hash.asJson))
    JsonObject(
      "id"     := id,
      "result" := JsonObject("tx_blob" := txBlob, "tx_json" := sortedTx),
      "status" := "success",
      "type"   := "response"
    )

  }

  /**
    *
    * @param signed
    * @return submit_multisigned request. No ID, and Sequence based on Sequence in head of signed list.
    *         hash field may be incorrect, not sure it will make a difference.
    */
  def createSubmitMultiSignedRq(signed: List[JsonObject]): Either[Throwable, JsonObject] = {
    SignFor.mergeMultipleFullResponses(signed).map { txjson =>
      JsonObject(
        "command" := "submit_multisigned",
        "tx_json" := txjson
      )
    }

  }
}
