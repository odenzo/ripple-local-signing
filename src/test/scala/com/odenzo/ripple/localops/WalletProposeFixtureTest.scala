package com.odenzo.ripple.localops

import cats._
import cats.data._
import io.circe.JsonObject
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.impl.crypto.RippleFormatConverters
import com.odenzo.ripple.localops.impl.crypto.core.ED25519CryptoBC
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.testkit.{FixtureUtils, JsonReqRes, OTestSpec}

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
    getOrLog(accountpubkey2address(pubKeyBytes)).v shouldEqual kAccount

    // Now it is keytype dependant to derive the public AccountKey from the Family Private Key / Generator
    kKeyType match {
      case "ed25519"   => checkDerivedKeyEd(kSeedHex, kPublicKeyHex)
      case "secp256k1" => ()
      case other       => fail(s"Unknown KeyType [$other]")
    }

    ()
  }

  def checkDerivedKeyEd(seedHex: String, accountPublicKeyHex: String): Unit = {
    // Okay, this is the mystery. We DO NOT use AccountFamily generator etc.
    // I *think* this is the account keypair for ed
    val keypair = getOrLog(ED25519CryptoBC.generateKeyPairFromHex(seedHex))
    val pubHex  = getOrLog(ED25519CryptoBC.publicKey2Hex(keypair.getPublic))
    pubHex shouldEqual accountPublicKeyHex
    ()

  }

  /** WalletPropose Rq and Rs for any key_typeds
    * Assumes they are all positive success cases.
    * */
  def doWalletFixture(resource: String): Either[AppError, List[Unit]] = {
    loadAndExecuteFixture(resource) { rr: JsonReqRes =>
      findObjectField("result", rr.rs).foreach(testKeyAgnostic)
    }
  }

  test("SECP Fixture") {
    getOrLog(doWalletFixture("/test/myTestData/keysAndTxn/secp256k1_wallets.json"))

  }
  test("ED Fixture") {
    getOrLog(doWalletFixture("/test/myTestData/keysAndTxn/ed25519_wallets.json"))

  }
}
