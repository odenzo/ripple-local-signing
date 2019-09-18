package com.odenzo.ripple.localops.impl.messagehandlers

import io.circe.Json
import io.circe.syntax._

import cats.implicits._

import com.odenzo.ripple.localops.LocalOpsError
import com.odenzo.ripple.localops.impl.WalletGenerator
import com.odenzo.ripple.localops.impl.utils.JsonUtils
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
  def propose(walletProposeRq: Json): Either[Json, Json] = {
    val result: Either[LocalOpsError, WalletProposeResult] = LocalOpsError.wrap("wallet propose") {

      val keyType = findFieldAsString("key_type", walletProposeRq).toOption.getOrElse("secp256k1")

      val zeroOrOne: List[(String, String)] = List("passphrase", "seed", "seed_hex")
        .fproduct(v => findFieldAsString(v, walletProposeRq).toOption)
        .collect {
          case (name, Some(v)) => (name, v)
        }

      val seed: Either[LocalOpsError, IndexedSeq[Byte]] = zeroOrOne match {
        case Nil                      => WalletGenerator.generateSeed()
        case ("seed", v) :: Nil       => WalletGenerator.generateSeedFromSeedB58(v)
        case ("seed_hex", v) :: Nil   => WalletGenerator.generateSeedFromHex(v)
        case ("passphrase", v) :: Nil => WalletGenerator.generateSeedBySniffing(v)
        case _                        => LocalOpsError("At most one of [passphrase, seed, seed_hex] may be supplied.").asLeft
      }

      seed.flatMap { seedBytes =>
        keyType.toLowerCase match {
          case "ed25519"   => WalletGenerator.generateEdKeys(seedBytes)
          case "secp256k1" => WalletGenerator.generateSecpKeys(seedBytes)
          case otherType   => LocalOpsError(s"Illegal key_type $otherType was not ed25519 or secp256k1").asLeft
        }
      }

    }
    val rqId: Option[Json] = findField("id", walletProposeRq).toOption

    result match {
      case Right(res)                => buildSuccessResponse(res.asJson, rqId).asRight
      case Left(rerr: ResponseError) => buildFailureResponse(walletProposeRq, rerr).asLeft
      case Left(err: LocalOpsError)  => buildFailureResponse(walletProposeRq, ResponseError.invalid(err.msg)).asLeft
    }
  }

}

object WalletProposeMsg extends WalletProposeMsg
