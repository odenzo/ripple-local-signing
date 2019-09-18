package com.odenzo.ripple.localops.fixtures

import io.circe.Json

import cats._
import cats.data._
import cats.implicits._
import org.scalatest.Assertion
import scribe.{Level, Logging}

import com.odenzo.ripple.bincodec.EncodedSTObject
import com.odenzo.ripple.bincodec.testkit.JsonReqRes
import com.odenzo.ripple.localops.impl.BinCodecProxy
import com.odenzo.ripple.localops.impl.messagehandlers.SignForMsg
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.testkit.{FixtureUtils, OTestSpec}
import com.odenzo.ripple.localops.{LocalOpsError, MessageBasedAPI, RippleLocalAPI}

/**
  * Tests the RippleLocalAPI and fixtures of SignRq / SignRs
  * for single signed tranasctions.
  *
  * DOES NOT work on multisigns
  *
  * This tests signing and verification
  *
  */
class SignFixture$Test extends OTestSpec with ByteUtils with FixtureUtils with JsonUtils with Logging {

  def testSigningParts(rr: JsonReqRes): Unit = {
    setTestLogLevel(Level.Debug)
    val result         = findRequiredField("result", rr.rs)
    val kTxJson        = findRequiredField("tx_json", result)
    val kTxSig: String = findRequiredStringField("TxnSignature", kTxJson)
    val kTxBlob        = findRequiredStringField("tx_blob", result) // This has SigningPubKey in it?

    val key = getOrLog(
      SignForMsg
        .extractKey(rr.rq)
        .leftMap(re => LocalOpsError(s"${re.error}  Msg: ${re.error_message} ${re.error_code}"))
    )

    val txnsigFromRs: String     = getOrLog(RippleLocalAPI.signToTxnSignature(kTxJson, key))
    val txblobFromRs: String     = getOrLog(RippleLocalAPI.signTxn(kTxJson, key))
    val cTxBlob: EncodedSTObject = getOrLog(BinCodecProxy.binarySerialize(kTxJson))

    cTxBlob.toHex shouldEqual kTxBlob
    txblobFromRs shouldEqual kTxBlob
    txnsigFromRs shouldEqual kTxSig

    val verified = getOrLog(RippleLocalAPI.verify(kTxJson))
    verified shouldEqual true
    ()
  }

  private def doFullSignMessage(rr: JsonReqRes): Assertion = {
    val rs: Json = MessageBasedAPI.sign(rr.rq)
    rs shouldEqual removeDeprecated(rr.rs)
  }

  private def testAll(rr: JsonReqRes): Assertion = {
    testSigningParts(rr)
    doFullSignMessage(rr)
  }

  test("Some of All Txn") {
    mutePackages(List("com.odenzo.ripple.bincodec"))
    val secp = getOrLog(loadFixtureSubset("/test/myTestData/signrqrs/secp256k1_txn.json", 1, 2))
    val data = getOrLog(loadFixtureSubset("/test/myTestData/signrqrs/all_txns.json", 4, 2))
    executeFixture(secp)(testSigningParts)
    ()
  }

  test("All Txn") {

    val fixs = List(
      "/test/myTestData/signrqrs/secp256k1_txn.json",
      "/test/myTestData/signrqrs/all_txns",
      "/test/myTestData/signrqrs/ed25519_txn.json"
    )

    fixs.traverse { fix =>
      logger.info(s"Doing Fixture Resource: $fix")
      for {
        data <- loadRqRsResource(fix)
        _   = logger.info(s"Executed ${data.length} cases")
        res = data.map(testAll)
      } yield res
    }
    ()

  }
}
