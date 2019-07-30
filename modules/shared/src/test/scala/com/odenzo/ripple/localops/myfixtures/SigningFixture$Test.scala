package com.odenzo.ripple.localops.myfixtures

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import io.circe.syntax._
import scribe.{Level, Logging}

import com.odenzo.ripple.bincodec.{EncodedSTObject, RippleCodecAPI}
import com.odenzo.ripple.localops.{BinCodecProxy, RippleLocalAPI}
import com.odenzo.ripple.localops.impl.messagehandlers.SignForRqRsHandler
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.testkit.{FixtureUtils, OTestSpec}

/**
  *  Goes through some server signed txn and results and does local signing to check correct
  *  - Fixtures the responses are autofilled with Fee, Flag, Sequence, PubSigningKey etc.. sometimes the requests are
  *  not.
  */
class SigningFixture$Test extends OTestSpec with ByteUtils with FixtureUtils with JsonUtils with Logging {

  def testJustSigning(rq: JsonObject, rs: JsonObject): Unit = {
    // This assumes all required fields are filled in.
    logger.info(s"Signing Rq ${rq.asJson.spaces4}")
    logger.info(s"Signing Rs ${rs.asJson.spaces4}")
    val tx_jsonRq = findRequiredObject("tx_json", rq)

    // Need to sniff the correct key

    val result: JsonObject = findRequiredObject("result", rs)
    val kTxJson            = findRequiredObject("tx_json", result)
    val kTxSig: String     = findRequiredStringField("TxnSignature", kTxJson)
    val kTxBlob            = findRequiredStringField("tx_blob", result) // This has SigningPubKey in it?

    // val txnsigFromRq: String = getOrLog(RippleLocalAPI.sign(tx_jsonRq, seed, keyType))
    val key = getOrLog(
      SignForRqRsHandler
        .extractKey(rq)
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

  test("All SECP") {
    val secpData: List[(JsonObject, JsonObject)] =
      loadRequestResponses("/test/myTestData/keysAndTxn/secp256k1_txn.json")

    logger.info(s"Testing ${secpData.length} cases")
    secpData.foreach(v => testJustSigning(v._1, v._2))
  }

  test("All ED25519") {

    val edData: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/keysAndTxn/ed25519_txn.json")
    logger.info(s"Testing ${edData.length} cases")
    edData.foreach(v => testJustSigning(v._1, v._2))
  }

  test("Some Txn") {
    val data = loadRequestResponses("/test/myTestData/txnscenarios/all_txns.json").slice(1, 2)
    logger.info(s"Testing ${data.length} cases")
    data.foreach(v => testJustSigning(v._1, v._2))
  }

  test("All Txn") {
    val data: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/txnscenarios/all_txns.json")
    logger.info(s"Testing ${data.length} cases")
    data.foreach(v => testJustSigning(v._1, v._2))
  }

  test("ed master") {
    val edData = loadRequestResponses("/test/myTestData/txnscenarios/ed25519_none_txns.json")
    logger.info(s"Testing ${edData.length} cases")
    edData.foreach(v => testJustSigning(v._1, v._2))
  }

  test("ed ed") {
    val data = loadRequestResponses("/test/myTestData/txnscenarios/ed25519_regular_ed25519_txns.json")
    logger.info(s"Testing ${data.length} cases")
    data.foreach(v => testJustSigning(v._1, v._2))
  }

  test("ed secp") {
    val data = loadRequestResponses("/test/myTestData/txnscenarios/ed25519_regular_secp256k1_txns.json")
    logger.info(s"Testing ${data.length} cases")
    data.foreach(v => testJustSigning(v._1, v._2))
  }

}
