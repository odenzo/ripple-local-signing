package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe.{Json, JsonObject}
import io.circe.syntax._
import java.io

import com.odenzo.ripple.localops.RippleLocalAPI.{packSigningKey, packSigningKeyFromB58}
import com.odenzo.ripple.localops.SignRqRsHandler.extractKey
import com.odenzo.ripple.localops.crypto.RippleFormatConverters
import com.odenzo.ripple.localops.reference.HashPrefix
import com.odenzo.ripple.localops.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.utils.caterrors.{AppError, OError}

object SignRqRsHandler extends StrictLogging with JsonUtils with RippleFormatConverters {

  // Returned if no secret and no key_type/seed_val

  /*
       Sign will overwrite any SigningPubKey even if its wrong.
       But if key_type is present and no secret it still barks about missing secret

   */

  /**
    *
    * @param rq Full SignRq (SingingPubKey doesn't need to be filled)
    * @return Left is error response SignRs, Right is success response
    */
  def processSignRequest(rq: JsonObject): Either[JsonObject, JsonObject] = {

    //validateRequest(rq)
        
    val ok: Either[ResponseError, JsonObject] = for {
      valid   ← validateRequest(rq)
      key     <- extractKey(rq)
      tx_json ← JsonUtils.findObjectField("tx_json", rq).leftMap(err ⇒ ResponseError.kNoTxJson)
      withPubKey = tx_json.add("SigningPubKey", key.signPubKey.asJson)
      sig     <- Signer.signToTxnSignature(withPubKey, key).leftMap(err⇒ ResponseError.kBadSecret)

      txBlob  ← Signer.createSignedTxBlob(withPubKey, sig).leftMap(err ⇒ ResponseError.kBadSecret)
      blobhex = ByteUtils.bytes2hex(txBlob)
      hash    = Signer.createResponseHash(txBlob)
      success = buildSuccessResponse(rq, tx_json, sig, blobhex, hash, key.signPubKey)
    } yield success

    ok.leftMap(re ⇒ buildFailureResponse(rq, re))

  }


  def validateAutofillFields(tx_json:JsonObject): Either[OError, JsonObject] = {
    List("Sequence", "Fee")
    if (tx_json.contains("Sequence") && tx_json.contains("Fee")) tx_json.asRight
    else AppError(s"Sequence and Fee must be present in tx_json").asLeft
  }

  def validateRequest(rq: JsonObject): Either[ResponseError, String] = {
    JsonUtils
      .findStringField("command", rq)
      .leftMap(_ ⇒ ResponseError.kNoCommand)
      .flatMap {
        case "sign" ⇒ "sign".asRight
        case other  ⇒ ResponseError.kBadCommand.asLeft
      }
  }

  def buildFailureResponse(rq: JsonObject, err: ResponseError): JsonObject = {
    val sorted = JsonUtils.sortFields(rq) // Only top level, but good enough
    JsonObject(
      ("error", Json.fromString(err.error)),
      ("error_code", err.error_code.asJson),
      ("error_message", err.error_message.asJson),
      ("request", sorted.asJson),
      ("status", Json.fromString("error")),
      ("type", Json.fromString("response"))
    )

  }
  def buildSuccessResponse(rq: JsonObject,
                           rqTxJson: JsonObject,
                           sig: TxnSignature,
                           signedBlob: String,
                           hash: String,
                           signPubKey: String): JsonObject = {

    val rsTxJson: JsonObject = rqTxJson
      .remove("SigningPubKey")
      .add("SigningPubKey", signPubKey.asJson)
      .add("TxnSignature", Json.fromString(sig.hex))
      .add("hash", Json.fromString(hash))

    val sortedTxJson = sortDeepFields(rsTxJson).asJson
    val result       = JsonObject(("tx_blob", signedBlob.asJson), ("tx_json", sortedTxJson))

    JsonObject
      .singleton("id", rq("id").getOrElse(Json.Null))
      .add("result", result.asJson)
      .add("status", Json.fromString("success"))
      .add("type", Json.fromString("response"))
  }

  def getFieldAsString(fieldName: String, obj: JsonObject): Option[(String, String)] = {
    JsonUtils.findStringField(fieldName, obj).toOption.map(v ⇒ (fieldName, v))
  }

  def extractKey(json: JsonObject): Either[ResponseError, SigningKey] = {

    // All the potential fields that are present as String
    val params: Map[String, String] = List("secret", "key_type", "seed", "seed_hex", "passphrase")
      .flatMap(getFieldAsString(_, json))
      .toMap

    val fieldsPresent = params.keys
    val keycount      = fieldsPresent.count(_ =!= "key_type")
    if (fieldsPresent.count(f ⇒ f === "key_type" || f === "secret") > 1) ResponseError.kSecretAndType.asLeft
    else if (keycount > 1) ResponseError.kTooMany.asLeft
    else if (keycount === 0) ResponseError.kNoSecret.asLeft // Even if key_type is present
    else {
      // We know we have the correct # paremeter fields now, execept key_type maybe missing
      params.get("secret").fold(explicitKey(params))(secretKey(_, params))
    }
  }

  protected def secretKey(secretB58: String, params: Map[String, String]): Either[ResponseError, SigningKey] = {
    convertBase58Check2hex(secretB58)
      .flatMap(packSigningKey(_, "secp256k1"))
      .leftMap(ae ⇒ ResponseError.kBadSecret)
  }

  protected def explicitKey(params: Map[String, String]): Either[ResponseError, SigningKey] = {

    params.get("key_type") match {
      case None ⇒ ResponseError.kNoSecret.asLeft // Mimicing Ripple even if there are seed, seed_hex passphrase
      case Some(kt) ⇒
        logger.info(s"Explicit Key Type $kt from $params")
        List(
          params.get("passphrase").map(convertPassphrase2hex),
          params.get("seed").map(convertBase58Check2hex),
          params.get("seed_hex").map(_.asRight[AppError])
        ).flatten.head
          .flatMap(packSigningKey(_, kt))
          .leftMap(ae ⇒ ResponseError.kBadSecret)
    }
  }
}
