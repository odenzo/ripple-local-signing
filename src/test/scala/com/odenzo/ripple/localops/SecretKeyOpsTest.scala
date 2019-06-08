package com.odenzo.ripple.localops

import io.circe.Decoder.Result
import org.scalatest.FunSuite
import io.circe._
import io.circe.syntax._

import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.models.{ED25519, KeyType, SECP256K1, WalletProposeResult}
import com.odenzo.ripple.localops.testkit.OTestSpec

/**
  *  Load wallet creation fixture and test each function
  */
class SecretKeyOpsTest extends OTestSpec with SecretKeyOps {

  test("Generate Keys") {
    val (edMaster, edReg)       = getOrLog(super.generateKeys(ED25519))
    val (secMaster, secRegular) = getOrLog(generateKeys(SECP256K1))

    getOrLog(doOneTest(edMaster))
    getOrLog(doOneTest(secMaster))

  }

  test("CODEC") {

    val err: Result[KeyType] = "badApple".asJson.as[KeyType]
    err.isLeft shouldBe true

    val canonEd = "ed25519".asJson
    "ED25519".asJson.as[KeyType] shouldEqual Right(ED25519)
    canonEd.as[KeyType] shouldEqual Right(ED25519)

    val canonSecp = "secp256k1".asJson
    "SECP256k1".asJson.as[KeyType] shouldEqual Right(SECP256K1)
    canonSecp.as[KeyType] shouldEqual Right(SECP256K1)

    val ed: KeyType = ED25519
    ed.asJson shouldEqual canonEd
    (SECP256K1: KeyType).asJson shouldEqual canonSecp

  }

  def doOneTest(wallet: WalletProposeResult) = {
    packSigningKey(wallet.master_seed_hex, wallet.key_type)
    packSigningKeyFromB58(wallet.master_seed, wallet.key_type)
    packSigningKeyFromRFC1751(wallet.master_key, wallet.key_type)
  }

}
