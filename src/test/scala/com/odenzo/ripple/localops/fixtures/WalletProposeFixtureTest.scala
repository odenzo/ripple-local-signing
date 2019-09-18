package com.odenzo.ripple.localops.fixtures

import io.circe.Json
import cats._
import cats.data._
import cats.implicits._

import org.scalatest.FunSuite
import scribe.Level

import com.odenzo.ripple.bincodec.testkit.JsonReqRes
import com.odenzo.ripple.localops.{LocalOpsError, MessageBasedAPI}
import com.odenzo.ripple.localops.impl.crypto.RippleFormatConverters
import com.odenzo.ripple.localops.impl.crypto.core.ED25519CryptoBC
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.testkit.{FixtureUtils, OTestSpec}

/**
  *  Need more than non-trivial requests. This exercises some internals plus the Public Message API
  *  processing same requests.
  */
class WalletProposeFixtureTest
    extends FunSuite
    with OTestSpec
    with FixtureUtils
    with JsonUtils
    with RippleFormatConverters {

  /** This is used to test a variety of RippleFormat things */
  def testKeyAgnostic(w: Json): Unit = {
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
    val keypair = getOrLog(ED25519CryptoBC.generateKeyPairFromHex(seedHex))
    val pubHex  = getOrLog(ED25519CryptoBC.publicKey2Hex(keypair.getPublic))
    pubHex shouldEqual accountPublicKeyHex
    ()

  }

  /** Message based should be ok if a seed of some type is passed in else skip this as randmon generation
    * is only testable that it succeeded. */
  def messageApi(rr: JsonReqRes) = {

    val seedCount: Int = List("seed", "seed_hex", "passphrase").count(v => findFieldAsString(v, rr.rq).isRight)

    val rs = MessageBasedAPI.walletPropose(rr.rq)
    logger.debug(s"In Request: ${rs.spaces4}")
    if (seedCount > 0) {
      rs shouldEqual removeDeprecated(rr.rs)
    } else {
      // Can only check it succeeded.
      findFieldAsString("status", rs) shouldEqual Right("success")
    }
  }

  /** WalletPropose Rq and Rs for any key_typeds
    * Assumes they are all positive success cases.
    * */
  def doWalletFixture(resource: String): Either[LocalOpsError, List[Unit]] = {
    loadAndExecuteFixture(resource) { rr: JsonReqRes =>
      findField("result", rr.rs).foreach(testKeyAgnostic)
      messageApi(rr)
    }
  }

  test("SECP Fixture") {
    setTestLogLevel(Level.Debug)
    getOrLog(doWalletFixture("/test/myTestData/wallets/secp256k1_wallets.json"))

  }
  test("ED Fixture") {
    getOrLog(doWalletFixture("/test/myTestData/wallets/ed25519_wallets.json"))

  }
}
