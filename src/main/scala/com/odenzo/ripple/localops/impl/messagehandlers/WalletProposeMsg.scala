package com.odenzo.ripple.localops.impl.messagehandlers

import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}

import com.odenzo.ripple.localops.impl.WalletGenerator
import com.odenzo.ripple.localops.impl.utils.JsonUtils
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.models.{ResponseError, WalletProposeResult}

/**
  * Mimic XRPL WebSocket API, accepting the full request for Wallet Propose
  */
trait WalletProposeMsg extends HandlerBase with JsonUtils {

  /**
    *
    * @param walletProposeRq Top Level full object with a Request
    * @return Top Level Response object, on left if error right is success
    */
  def propose(walletProposeRq: JsonObject): Either[JsonObject, JsonObject] = {
    val result: Either[AppError, WalletProposeResult] = AppError.wrap("wallet propose") {
      val atMostOne = List("passphrase", "seed", "seed_hex")

      val found: List[(String, String)] = atMostOne.flatMap(k => walletProposeRq(k).flatMap(_.asString).tupleLeft(k))
      val seed: Either[AppError, IndexedSeq[Byte]] = found match {
        case Nil                      => WalletGenerator.generateSeed()
        case ("seed", v) :: Nil       => WalletGenerator.generateSeedFromSeedB58(v)
        case ("seed_hex", v) :: Nil   => WalletGenerator.generateSeedFromHex(v)
        case ("passphrase", v) :: Nil => WalletGenerator.generateSeedBySniffing(v)
        case _                        => AppError("At most one of [passphrase, seed, seed_hex] may be supplied.").asLeft
      }

      seed.flatMap { seedBytes =>
        findStringField("key_type", walletProposeRq) match {
          case Left(err)          => AppError("IllegalArgumentException - no key_type").asLeft
          case Right("ed25519")   => WalletGenerator.generateEdKeys(seedBytes)
          case Right("secp256k1") => WalletGenerator.generateSecpKeys(seedBytes)
          case Right(otherType)   => AppError(s"Illegal key_type $otherType was not ed25519 or secp256k1").asLeft
        }
      }

    }
    val rqId: Option[Json] = walletProposeRq("id")

    result match {
      case Right(res)                => buildSuccessResponse(res.asJsonObject, rqId).asRight
      case Left(rerr: ResponseError) => buildFailureResponse(walletProposeRq.asJson, rerr).asLeft
      case Left(err: AppError)       => buildFailureResponse(walletProposeRq.asJson, ResponseError.invalid(err.msg)).asLeft
    }
  }

}

object WalletProposeMsg extends WalletProposeMsg
