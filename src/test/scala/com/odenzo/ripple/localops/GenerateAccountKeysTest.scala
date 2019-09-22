package com.odenzo.ripple.localops

import io.circe.Decoder.Result
import io.circe.{Decoder, Json}
import io.circe.syntax._

import com.odenzo.ripple.localops.impl.utils.JsonUtils
import com.odenzo.ripple.localops.models._
import com.odenzo.ripple.localops.testkit.OTestSpec

class GenerateAccountKeysTest extends OTestSpec with RippleLocalAPI {

  test("Generate Keys") {
    val edReg: Json    = getOrLog(super.generateAccountKeys(ED25519.txt))
    val edMaster: Json = getOrLog(super.generateAccountKeys(ED25519.txt))

    val secMaster  = getOrLog(generateAccountKeys(SECP256K1.txt))
    val secRegular = getOrLog(generateAccountKeys(SECP256K1.txt))

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

  def doOneTest(json: Json): Either[LocalOpsError, SigningKey] = {
    for {
      wallet <- JsonUtils.decode(json, Decoder[WalletProposeResult])
      -      <- packSigningKey(wallet.master_seed_hex, wallet.key_type.txt)
      _      <- packSigningKeyFromB58(wallet.master_seed, wallet.key_type.txt)
      sk     <- packSigningKeyFromRFC1751(wallet.master_key, wallet.key_type.txt)
    } yield sk
  }

}
