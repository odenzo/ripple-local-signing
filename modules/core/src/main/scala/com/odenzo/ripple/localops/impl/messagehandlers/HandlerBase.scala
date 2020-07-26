package com.odenzo.ripple.localops.impl.messagehandlers

import io.circe.syntax._
import io.circe.{Json, JsonObject}

import cats.implicits._
import scribe.Logging

import com.odenzo.ripple.localops.impl.crypto.RippleFormatConverters
import com.odenzo.ripple.localops.impl.utils.JsonUtils
import com.odenzo.ripple.localops.models.{ResponseError, SECP256K1, SigningKey}
import com.odenzo.ripple.localops.{LocalOpsError, RippleLocalAPI}

trait HandlerBase extends Logging with RippleFormatConverters with JsonUtils {

  def validateAutofillFields(tx_json: Json): Either[ResponseError, Json] = {
    val requiredFields = List("Sequence", "Fee")
    requiredFields.forall(findField(_, tx_json).isRight) match {
      case true  => tx_json.asRight
      case false => ResponseError(s"Sequence and Fee must be present in tx_json", None, None).asLeft
    }
  }

  def validateCommandCorrect(expCommand: String, rq: Json): Either[ResponseError, String] = {
    for {
      cmd     <- JsonUtils.findFieldAsString("command", rq).leftMap(_ => ResponseError.kNoCommand)
      correct <- Either.cond(cmd === expCommand, cmd, ResponseError.kBadCommand)
    } yield correct
  }

  def buildFailureResponse(rq: Json, err: ResponseError): Json = {
    JsonObject(
      "error"         := err.error,
      "error_code"    := err.error_code,
      "error_message" := err.error_message,
      "request"       := rq, // Ripple sorts these, I don't here. At final point could when going to string
      "status"        := "error",
      "type"          := "response"
    ).asJson
  }

  /**
    *
    * @param result
    * @param id
    *
    * @return
    */
  def buildSuccessResponse(result: Json, id: Option[Json] = None): Json = {

    val done = JsonObject(
      "id"     := id,
      "result" := result.mapObject(JsonUtils.sortFieldsDroppingNulls),
      "status" := "success",
      "type"   := "response"
    )
    JsonUtils.dropNullValues(done).asJson
  }

  def buildTxnSuccessResultField(txJson: Json, txBlob: String, hash: String): Json = {

    val fulltx: Json = txJson.mapObject(_.add("hash", hash.asJson))

    // TODO: Not sorted alphabetically yet
    val sortedTx = fulltx.dropNullValues
    JsonObject(
      "tx_blob" := txBlob,
      "tx_json" := sortedTx
    ).asJson

  }

  /**
    *   Get the key in whatever format from the Sign / SignFor message. Should be the same for multisign
    * @param json
    * @return
    */
  def extractKey(json: Json): Either[ResponseError, SigningKey] = {

    // There should be exactly one of these.
    val exactOnce: List[(String, String)] = List("secret", "seed", "seed_hex", "passphrase")
      .fproduct(v => findFieldAsString(v, json).toOption)
      .collect {
        case (name, Some(v)) => (name, v)
      }

    val keyType                                       = findFieldAsString("key_type", json).toOption
    val rec: List[((String, String), Option[String])] = exactOnce.tupleRight(keyType)

    rec match {
      case list if list.isEmpty    => ResponseError.kNoSecret.asLeft
      case list if list.length > 1 => ResponseError.kTooMany.asLeft

      case (("secret", _), Some(_)) :: Nil => ResponseError.kNoSecret.asLeft
      case (("secret", v), None) :: Nil    => secretKey(v)

      case ((_, _), None) :: Nil         => ResponseError.kNoSecret.asLeft
      case ((fName, v), Some(kt)) :: Nil => explicitKey(fName, v, kt)
    }

  }

  /** THis is when secret field is used in signing request, and only applicable for SECP256K1 keys */
  protected def secretKey(secretB58: String): Either[ResponseError, SigningKey] = {
    convertBase58Check2hex(secretB58)
      .flatMap(RippleLocalAPI.packSigningKey(_, SECP256K1.txt))
      .leftMap(ae => ResponseError.kBadSecret)
  }

  protected def explicitKey(name: String, keyVal: String, keyType: String): Either[ResponseError, SigningKey] = {

    val asHex: Either[LocalOpsError, String] = name match {
      case "passphrase" => convertPassword2hex(keyVal)
      case "seed"       => convertBase58Check2hex(keyVal)
      case "seed_hex"   => keyVal.asRight
      case other        => LocalOpsError(s"Unexpected Key Parameters $other").asLeft
    }

    asHex
      .flatMap(v => RippleLocalAPI.packSigningKey(v, keyType))
      .leftMap(_ => ResponseError(s"Internal Error Packing Key", None, None))
  }
}
