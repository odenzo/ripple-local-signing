package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.crypto.core.{ED25519CryptoBC, Secp256K1CryptoBC}
import com.odenzo.ripple.localops.crypto.{AccountFamily, RippleFormatConverters}
import com.odenzo.ripple.localops.utils.caterrors.CatsTransformers.ErrorOr
import com.odenzo.ripple.localops.utils.{ByteUtils, CirceUtils, FixtureUtils, JsonUtils}

/** This tests functionality for wallet generation, WalletPropose.
  * It exercises the routines to generate account key pairs from seed and other conversions.
  * Covers secp256k1 and ed25519 key types.
  * Note the master_key is for AccountFamily and the public key is for the Account
  */
class WalletGeneratorTest extends FunSuite with OTestSpec with FixtureUtils with JsonUtils with RippleFormatConverters {

  private val edWallet: String =
    """
    {
      |        "account_id": "rn4gsh2qp8842mTA5HfwGT3L1XepQCpqiu",
      |        "key_type": "ed25519",
      |        "master_key": "ANTE TUFT MEG CHEN CRAB DUMB COW OWNS ROOF FRED EDDY FORD",
      |        "master_seed": "spqnjaMMxPSvtaD4nevqqdjj4kzie",
      |        "master_seed_hex": "09A117434757F90BF0BED6B29F185E4D",
      |        "public_key": "aKGGHoqb2C2Xj6qtzikTTdsQdPcnYS8ue4XzXvT2T6fuofFP4zrA",
      |        "public_key_hex": "EDC5349AD8114DCDA07A355AA850FABE710CEE8FCBD891F1B919A6F6713C7BABA0"
      |      }
    """.stripMargin

 private  val secpWallet: String = """{
                     |        "account_id": "rnnZjcheRzS5qDMeYnqxaLWAnNCSVJgZud",
                     |        "key_type": "secp256k1",
                     |        "master_key": "AT CLOT LOSS FORE BEST KOCH TRAY TONG TOOK NULL BOOT IS",
                     |        "master_seed": "ssZEpYD1cWaA1CWn51hiDZg2KXyRX",
                     |        "master_seed_hex": "3DCE3563B7B33DEF506F75C2CBFE0C04",
                     |        "public_key": "aBRQsmHX2XFCEdQA65C6i2sbR5cbccmD8AZ3hEV9ncce1voL7WmG",
                     |        "public_key_hex": "03C518E83306F821373E801C865A8F7B9150A6D1EA5174C6364DABC9CADEA34A05"
                     |      }""".stripMargin

  private  val ed: ErrorOr[JsonObject]   = CirceUtils.parseAsJsonObject(edWallet)
  private val secp: ErrorOr[JsonObject] = CirceUtils.parseAsJsonObject(secpWallet)

  def testKeyAgnostic(w: JsonObject): Unit = {
    val kMaster       = findRequiredStringField("master_key", w)
    val kSeed         = findRequiredStringField("master_seed", w)
    val kSeedHex      = findRequiredStringField("master_seed_hex", w)
    val kPublicKey    = findRequiredStringField("public_key", w)
    val kPublicKeyHex = findRequiredStringField("public_key_hex", w)
    val kAccount      = findRequiredStringField("account_id", w)
    val kKeyType      = findRequiredStringField("key_type", w)

    convertMasterKey2masterSeedHex(kMaster).right.value shouldEqual kSeedHex

    val rSeedHex =
      getOrLog(convertBase58Check2hex(kSeed), s"Converting MasterSeed 2 Hex $kSeed")
    rSeedHex shouldEqual kSeedHex

    val pub58Hex = getOrLog(convertBase58Check2hex(kPublicKey))
    pub58Hex shouldEqual kPublicKeyHex

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

  test("secp single") {
    getOrLog(secp)
    secp.foreach(testKeyAgnostic)
  }
  test("ed single") {
    getOrLog(ed)
    ed.foreach(testKeyAgnostic)
  }

  test("SECP Fixture") {
    doWalletFixture("/test/myTestData/keysAndTxn/secp256k1_wallets.json")

  }
  test("ED Fixture") {
    doWalletFixture("/test/myTestData/keysAndTxn/ed25519_wallets.json")

  }
}
