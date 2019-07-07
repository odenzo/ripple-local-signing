package com.odenzo.ripple.localops.myfixtures

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.crypto.core.{ED25519CryptoBC, Secp256K1CryptoBC}
import com.odenzo.ripple.localops.crypto.{AccountFamily, RippleFormatConverters}
import com.odenzo.ripple.localops.testkit.{FixtureUtils, OTestSpec}
import com.odenzo.ripple.localops.utils.caterrors.CatsTransformers.ErrorOr
import com.odenzo.ripple.localops.utils.{ByteUtils, CirceUtils, JsonUtils}

/**
  * * TODO: Implement true WalletPropose local and test sample responses with me processing same requests.
  */
class WalletProposeFixtureTest
    extends FunSuite
    with OTestSpec
    with FixtureUtils
    with JsonUtils
    with RippleFormatConverters {

  /** This is used to test a variety of RippleFormat things */
  def testKeyAgnostic(w: JsonObject): Unit = {
    val kMaster       = findRequiredStringField("master_key", w)
    val kSeed         = findRequiredStringField("master_seed", w)
    val kSeedHex      = findRequiredStringField("master_seed_hex", w)
    val kPublicKey    = findRequiredStringField("public_key", w)
    val kPublicKeyHex = findRequiredStringField("public_key_hex", w)
    val kAccount      = findRequiredStringField("account_id", w)
    val kKeyType      = findRequiredStringField("key_type", w)

    getOrLog(convertMasterKey2masterSeedHex(kMaster)) shouldEqual kSeedHex

    getOrLog(convertBase58Check2hex(kSeed), s" MasterSeed 2 Hex $kSeed") shouldEqual kSeedHex

    getOrLog(convertBase58Check2hex(kPublicKey)) shouldEqual kPublicKeyHex

    val pubKeyBytes = getOrLog(ByteUtils.hex2bytes(kPublicKeyHex))
    accountpubkey2address(pubKeyBytes) shouldEqual kAccount

    // Now it is keytype dependant to derive the public AccountKey from the Family Private Key / Generator
    kKeyType match {
      case "ed25519"   ⇒ checkDerivedKeyEd(kSeedHex, kPublicKeyHex)
      case "secp256k1" ⇒ ()
      case other       ⇒ fail(s"Unknown KeyType [$other]")
    }

    ()
  }

  def checkDerivedKeyEd(seedHex: String, accountPublicKeyHex: String): Unit = {
    // Okay, this is the mystery. We DO NOT use AccountFamily generator etc.
    // I *think* this is the account keypair for ed
    val keypair = getOrLog(ED25519CryptoBC.seedHex2keypair(seedHex))
    val pubHex  = getOrLog(ED25519CryptoBC.publicKey2Hex(keypair.getPublic))
    pubHex shouldEqual accountPublicKeyHex
    ()

  }


  /** WalletPropose Rq and Rs for any key_typeds
    * Assumes they are all positive success cases.
    * */
  def doWalletFixture(resource: String): Unit = {
    val loadResults = loadRequestResponses(resource).map(_._2).traverse(rs ⇒ findObjectField("result", rs))

    val results = getOrLog(loadResults, s"Trouble Getting Result fields")

    results.foreach { result ⇒
      // logger.debug(s"Testing Result: ${result.asJson.spaces4}")
      testKeyAgnostic(result)
    }
    ()
  }

  test("SECP Fixture") {
    doWalletFixture("/test/myTestData/keysAndTxn/secp256k1_wallets.json")

  }
  test("ED Fixture") {
    doWalletFixture("/test/myTestData/keysAndTxn/ed25519_wallets.json")

  }
}
