package com.odenzo.ripple.localops.handlers

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import scribe.Logging

import com.odenzo.ripple.localops.RippleLocalOps.packSigningKey
import com.odenzo.ripple.localops.crypto.RippleFormatConverters
import com.odenzo.ripple.localops.utils.caterrors.{AppError, OError}
import com.odenzo.ripple.localops.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.{KeyType, ResponseError, SECP256K1, Signer, SigningKey, TxnSignature}

object SignRqRsHandler extends Logging with JsonUtils with RippleFormatConverters {

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
      sig <- Signer.signToTxnSignature(withPubKey, key).leftMap(err ⇒ ResponseError.kBadSecret)

      txBlob ← Signer.createSignedTxBlob(withPubKey, sig).leftMap(err ⇒ ResponseError.kBadSecret)
      blobhex = ByteUtils.bytes2hex(txBlob)
      hash    = Signer.createResponseHash(txBlob)
      success = buildSuccessResponse(rq, tx_json, sig, blobhex, hash, key.signPubKey)
    } yield success

    ok.leftMap(re ⇒ buildFailureResponse(rq, re))

  }

  def validateAutofillFields(tx_json: JsonObject): Either[OError, JsonObject] = {
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

  def extractKey(json: JsonObject): Either[ResponseError, SigningKey] = {

    // All the potential fields that are present as String
    val params: Map[String, String] = List("secret", "key_type", "seed", "seed_hex", "passphrase")
      .flatMap(getFieldAsString(_, json))
      .toMap

    val fieldsPresent = params.keys
    val keycount      = fieldsPresent.count(_ =!= "key_type")
    if (fieldsPresent.count(f ⇒ f === "key_type" || f === "secret") > 1) ResponseError.kSecretAndType.asLeft
    else
      keycount match {
        case 1 ⇒
          // We know we have the correct # paremeter fields now, execept key_type maybe missing
          params.get("secret").fold(explicitKey(params))(secretKey(_, params))

        case 0     ⇒ ResponseError.kNoSecret.asLeft // Even if key_type is present
        case other ⇒ ResponseError.kTooMany.asLeft
      }
  }

  /** THis is when secret field is used in signing request, and only applicable for SECP256K1 keys */
  protected def secretKey(secretB58: String, params: Map[String, String]): Either[ResponseError, SigningKey] = {
    convertBase58Check2hex(secretB58)
      .flatMap(packSigningKey(_, SECP256K1))
      .leftMap(ae ⇒ ResponseError.kBadSecret)
  }

  protected def explicitKey(params: Map[String, String]): Either[ResponseError, SigningKey] = {

    val keyType = params.get("key_type") match {
      case None     ⇒ ResponseError.kNoSecret.asLeft // Mimicing Ripple even if there are seed, seed_hex passphrase
      case Some(kt) ⇒ KeyType.fromText(kt)
    }

    keyType.flatMap { kt ⇒
      logger.debug(s"Explicit Key Type $kt from $params")

      val shouldBeOne: List[Either[AppError, String]] = List(
        params.get("passphrase").map(convertPassword2hex),
        params.get("seed").map(convertBase58Check2hex),
        params.get("seed_hex").map(_.asRight[AppError])
      ).flatten
      val exactlyOne: Either[AppError, SigningKey] = shouldBeOne match { // Exactly one check
        case first :: Nil ⇒ first.flatMap((v: String) ⇒ packSigningKey(v, kt))
        case other        ⇒ AppError("Not Exactly One passphrease,seed, seed_hex").asLeft[SigningKey]
      }
      exactlyOne.leftMap(e ⇒ ResponseError.kNoSecret) // Throwing away a potential useful error
    }
  }
}