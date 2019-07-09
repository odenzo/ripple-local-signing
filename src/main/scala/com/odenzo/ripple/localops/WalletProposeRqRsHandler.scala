package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import io.circe.{Json, JsonObject}
import io.circe.syntax._
import java.io
import java.security.SecureRandom

import scribe.Logging

import com.odenzo.ripple.localops.WalletGenerator.logger
import com.odenzo.ripple.localops.crypto.RippleFormatConverters
import com.odenzo.ripple.localops.utils.{ByteUtils, CirceUtils, JsonUtils}
import com.odenzo.ripple.localops.utils.caterrors.AppError

/**
  * This is for the case where we wish to mimic XRPL WebSocket API
  */
trait WalletProposeRqRsHandler extends Logging with JsonUtils {

  def propose(walletProposeRq: JsonObject) = {

    val atMostOne = List("passphrase", "seed", "seed_hex")

    val found: List[(String, String)] = atMostOne.flatMap(k ⇒ walletProposeRq(k).flatMap(_.asString).tupleLeft(k))
    val seed: Either[AppError, List[Byte]] = found match {
      case Nil                      ⇒ WalletGenerator.generateSeed()
      case ("seed", v) :: Nil       ⇒ WalletGenerator.generateSeedFromSeedB58(v)
      case ("seed_hex", v) :: Nil   ⇒ WalletGenerator.generateSeedFromHex(v)
      case ("passphrase", v) :: Nil ⇒ WalletGenerator.generateSeedBySniffing(v)
      case _                        ⇒ AppError("At most one of [passphrase, seed, seed_hex] may be supplied.").asLeft
    }

    seed.flatMap { seedBytes ⇒
      findStringField("key_type", walletProposeRq) match {
        case Left(err)          ⇒ AppError("IllegalArgumentException - no key_type").asLeft
        case Right("ed25519")   ⇒ WalletGenerator.generateEdKeys(seedBytes)
        case Right("secp256k1") ⇒ WalletGenerator.generateSecpKeys(seedBytes)
        case Right(otherType)   ⇒ AppError(s"Illegal key_type $otherType was not ed25519 or secp256k1").asLeft
      }
    }

    /*
    {
      "request": {
        "command": "wallet_propose",
        "key_type": "ed25519",
        "id": "7685e62f-0fd9-4661-bd54-2045675cbfb7"
      }
   */
  }

  /**
    *
    * @param result
    * @param id
    * @return
    */
  def buildSuccessResponse(result: WalletProposeResult, id: Option[String] = None): JsonObject = {

    val fields = Map(
      "id"     → id.asJson,
      "result" → result.asJson,
      "status" → Json.fromString("success"),
      "type"   → Json.fromString("response")
    )

    val obj = JsonObject.fromMap(fields)
    CirceUtils.sortDeepFields(obj) // Put all fields in alphabetical order per object

  }

  def buildErrorResponse(rq: JsonObject, err: AppError): JsonObject = {

    val fields: Map[String, Json] = Map(
      "error"   -> err.msg.asJson,
      "id"      → rq("id").asJson,
      "request" → rq.asJson,
      "status"  → "error".asJson,
      "type"    → "response".asJson
    )

    val obj = JsonObject.fromMap(fields)
    CirceUtils.sortDeepFields(obj) // Put all fields in alphabetical order per object
  }

}
