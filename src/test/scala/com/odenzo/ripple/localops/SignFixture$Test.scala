package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import scribe.Logging

import com.odenzo.ripple.bincodec.EncodedSTObject
import com.odenzo.ripple.localops.impl.BinCodecProxy
import com.odenzo.ripple.localops.impl.messagehandlers.SignForMsg
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.testkit.{FixtureUtils, JsonReqRes, OTestSpec}

/**
  * Tests the RippleLocalAPI signTxn and signToTxnSignature
  *
  */
class SignFixture$Test extends OTestSpec with ByteUtils with FixtureUtils with JsonUtils with Logging {

  def testJustSigning(rr: JsonReqRes): Unit = {

    val result: JsonObject = findRequiredObject("result", rr.rs)
    val kTxJson            = findRequiredObject("tx_json", result)
    val kTxSig: String     = findRequiredStringField("TxnSignature", kTxJson)
    val kTxBlob            = findRequiredStringField("tx_blob", result) // This has SigningPubKey in it?

    val key = getOrLog(
      SignForMsg
        .extractKey(rr.rq)
        .leftMap(re => AppError(s"${re.error}  Msg: ${re.error_message} ${re.error_code}"))
    )

    val txnsigFromRs: String     = getOrLog(RippleLocalAPI.signToTxnSignature(kTxJson, key))
    val txblobFromRs             = getOrLog(RippleLocalAPI.signTxn(kTxJson, key))
    val cTxBlob: EncodedSTObject = getOrLog(BinCodecProxy.binarySerialize(kTxJson))

    logger.info(s"=====\nGot/Excpted TxBlob: \n ${cTxBlob.toHex} \n $kTxBlob\n\n")
    cTxBlob.toHex shouldEqual kTxBlob

    txnsigFromRs shouldEqual kTxSig
    // txnsigFromRq shouldEqual txnsigFromRs   // If not then probably a field not populated (like SigningPubKey!)s

    ()
  }

  test("Some of All Txn") {
    val data = getOrLog(loadFixtureSubset("/test/myTestData/txnscenarios/all_txns.json", 4, 2))
    executeFixture(data)(testJustSigning)
    ()
  }

  test("All Txn") {
    val data = getOrLog(loadFixture("/test/myTestData/txnscenarios/all_txns.json"))
    executeFixture(data)(testJustSigning)
    ()

  }
}
