package com.odenzo.ripple.localops.impl.messagehandlers

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}

import com.odenzo.ripple.localops.impl.WalletGenerator
import com.odenzo.ripple.localops.impl.utils.JsonUtils
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.{ResponseError, WalletProposeResult}

/**
  * This is for the case where we wish to mimic XRPL WebSocket API
  */
trait WalletProposeRqRsHandler extends HandlerBase with JsonUtils {

  def propose(walletProposeRq: JsonObject): JsonObject = {

    val atMostOne = List("passphrase", "seed", "seed_hex")

    val found: List[(String, String)] = atMostOne.flatMap(k => walletProposeRq(k).flatMap(_.asString).tupleLeft(k))
    val seed: Either[AppError, List[Byte]] = found match {
      case Nil                      => WalletGenerator.generateSeed()
      case ("seed", v) :: Nil       => WalletGenerator.generateSeedFromSeedB58(v)
      case ("seed_hex", v) :: Nil   => WalletGenerator.generateSeedFromHex(v)
      case ("passphrase", v) :: Nil => WalletGenerator.generateSeedBySniffing(v)
      case _                        => AppError("At most one of [passphrase, seed, seed_hex] may be supplied.").asLeft
    }

    val result: Either[AppError, WalletProposeResult] = seed.flatMap { seedBytes =>
      findStringField("key_type", walletProposeRq) match {
        case Left(err)          => AppError("IllegalArgumentException - no key_type").asLeft
        case Right("ed25519")   => WalletGenerator.generateEdKeys(seedBytes)
        case Right("secp256k1") => WalletGenerator.generateSecpKeys(seedBytes)
        case Right(otherType)   => AppError(s"Illegal key_type $otherType was not ed25519 or secp256k1").asLeft
      }
    }

    val rqId: Option[Json] = walletProposeRq("id")

    result match {
      case Right(res)                => buildSuccessResponse(res.asJsonObject, rqId)
      case Left(rerr: ResponseError) => buildFailureResponse(walletProposeRq, rerr)
      case Left(err: AppError)       => buildFailureResponse(walletProposeRq, ResponseError.invalid(err.msg))
    }
  }

}

object WalletProposeRqRsHandler extends WalletProposeRqRsHandler
