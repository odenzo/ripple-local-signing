package com.odenzo.ripple.localops.impl.messagehandlers

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import scribe.Logging

import com.odenzo.ripple.localops._
import com.odenzo.ripple.localops.impl.Signer
import com.odenzo.ripple.localops.impl.crypto.RippleFormatConverters
import com.odenzo.ripple.localops.impl.utils.caterrors.{AppError, OError}
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}

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
object SignForRqRsHandler extends HandlerBase with Logging with JsonUtils with RippleFormatConverters {

  /**
    *
    * @param rq Full SignRq (SingingPubKey doesn't need to be filled)
    *
    * @return Left is error response SignRs, Right is success response
    */
  def signFor(rq: JsonObject): Either[JsonObject, JsonObject] = {

    val ok: Either[ResponseError, JsonObject] = for {
      _           <- validateRequest(rq)
      key         <- extractKey(rq) // One of the multisigners.
      signingAcct <- findStringField("account", rq).leftMap(err => ResponseError.invalid("Missing account field"))
      tx_json     <- JsonUtils.findObjectField("tx_json", rq).leftMap(err => ResponseError.kNoTxJson)
      sig         <- Signer.signForTxnSignature(tx_json, key, signingAcct).leftMap(err => ResponseError.kBadSecret)
      tx_jsonOut = Signer.createSuccessTxJson(tx_json, signingAcct, sig, key.signPubKey)
      txBlob <- BinCodecProxy.serialize(tx_jsonOut).leftMap(err => ResponseError.invalid(s"Internal Error ${err.msg}"))
      blobhex = ByteUtils.bytes2hex(txBlob)
      hash    = Signer.createResponseHashHex(txBlob.toIndexedSeq)
      success = buildSuccessResponse(rq("id"), tx_jsonOut, blobhex, hash)
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
    JsonObject(
      "id"     := id,
      "result" := JsonObject("tx_blob" := txBlob, "tx_json" := txJson.add("hash", hash.asJson)),
      "status" := "success",
      "type"   := "response"
    )

  }

}
