package com.odenzo.ripple.localops.impl.messagehandlers

import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import scribe.Logging

import com.odenzo.ripple.localops.SecretKeyOps
import com.odenzo.ripple.localops.impl.crypto.RippleFormatConverters
import com.odenzo.ripple.localops.impl.messagehandlers.SignForRqRsHandler.getFieldAsString
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.impl.utils.{CirceUtils, JsonUtils}
import com.odenzo.ripple.localops.models.{KeyType, ResponseError, SECP256K1, SigningKey}

trait HandlerBase extends Logging with RippleFormatConverters {

  def buildFailureResponse(rq: JsonObject, err: ResponseError): JsonObject = {
    val sorted = JsonUtils.sortFields(rq) // Only top level, but good enough
    JsonObject(
      "error"         := err.error,
      "error_code"    := err.error_code,
      "error_message" := err.error_message,
      "request"       := sorted,
      "status"        := "error",
      "type"          := "response"
    )

  }

  /**
    *
    * @param result
    * @param id
    *
    * @return
    */
  def buildSuccessResponse(result: JsonObject, id: Option[Json] = None): JsonObject = {

    val obj = JsonObject(
      "id"     := id,
      "result" := result,
      "status" := "success",
      "type"   := "response"
    )

    CirceUtils.sortDeepFields(obj) // Put all fields in alphabetical order per object

  }

  /**
    * SignRq and SignFor requests have same structure for the signing secreThis extracts
    *
    * @param json
    *
    * @return
    */
  def extractKey(json: JsonObject): Either[ResponseError, SigningKey] = {
    // All the potential fields that are present as String
    val params: Map[String, String] = List("secret", "key_type", "seed", "seed_hex", "passphrase")
      .flatMap(getFieldAsString(_, json))
      .toMap

    val fieldsPresent = params.keys
    val keycount      = fieldsPresent.count(_ =!= "key_type")
    if (fieldsPresent.count(f => f === "key_type" || f === "secret") > 1) ResponseError.kSecretAndType.asLeft
    else
      keycount match {
        case 1 =>
          // We know we have the correct # paremeter fields now, execept key_type maybe missing
          params.get("secret").fold(explicitKey(params))(secretKey(_, params))

        case 0     => ResponseError.kNoSecret.asLeft // Even if key_type is present
        case other => ResponseError.kTooMany.asLeft
      }
  }

  /** THis is when secret field is used in signing request, and only applicable for SECP256K1 keys */
  protected def secretKey(secretB58: String, params: Map[String, String]): Either[ResponseError, SigningKey] = {
    convertBase58Check2hex(secretB58)
      .flatMap(SecretKeyOps.packSigningKey(_, SECP256K1))
      .leftMap(ae => ResponseError.kBadSecret)
  }

  protected def explicitKey(params: Map[String, String]): Either[ResponseError, SigningKey] = {

    val keyType = params.get("key_type") match {
      case None     => ResponseError.kNoSecret.asLeft // Mimicing Ripple even if there are seed, seed_hex passphrase
      case Some(kt) => KeyType.fromText(kt)
    }

    keyType.flatMap { kt =>
      logger.debug(s"Explicit Key Type $kt from $params")

      val shouldBeOne: List[Either[AppError, String]] = List(
        params.get("passphrase").map(convertPassword2hex),
        params.get("seed").map(convertBase58Check2hex),
        params.get("seed_hex").map(_.asRight[AppError])
      ).flatten
      val exactlyOne: Either[AppError, SigningKey] = shouldBeOne match { // Exactly one check
        case first :: Nil => first.flatMap((v: String) => SecretKeyOps.packSigningKey(v, kt))
        case other        => AppError("Not Exactly One passphrease,seed, seed_hex").asLeft[SigningKey]
      }
      exactlyOne.leftMap(e => ResponseError.kNoSecret) // Throwing away a potential useful error
    }
  }
}
