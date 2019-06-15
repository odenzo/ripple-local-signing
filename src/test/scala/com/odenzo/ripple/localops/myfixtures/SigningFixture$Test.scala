package com.odenzo.ripple.localops.myfixtures

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import io.circe.syntax._
import org.scalatest.{Assertion, FunSuite}

import com.odenzo.ripple.bincodec.RippleCodecAPI
import com.odenzo.ripple.bincodec.serializing.BinarySerializer
import com.odenzo.ripple.localops.utils.caterrors.AppError
import com.odenzo.ripple.localops.utils.{ByteUtils, FixtureUtils, JsonUtils}
import com.odenzo.ripple.localops.{OTestSpec, RippleLocalAPI, TxnSignature}

/**
  *  Goes through some server signed txn and results and does local signing to check correct
  *  - Fixtures the responses are autofilled with Fee, Flag, Sequence, PubSigningKey etc.. sometimes the requests are
  *  not.
  */
class SigningFixture$Test extends FunSuite with OTestSpec with ByteUtils with FixtureUtils with JsonUtils {

  def testJustSigning(rq: JsonObject, rs: JsonObject): Unit = {
    // This assumes all required fields are filled in.
    logger.info(s"Signing Rq ${rq.asJson.spaces4}")
    logger.info(s"Signing Rs ${rs.asJson.spaces4}")
    val tx_jsonRq       = findRequiredObject("tx_json", rq)
    val seed: String    = findRequiredStringField("seed", rq)
    val keyType: String = findRequiredStringField("key_type", rq)

    val result: JsonObject = findRequiredObject("result", rs)
    val kTxJson            = findRequiredObject("tx_json", result)
    val kTxSig: String     = findRequiredStringField("TxnSignature", kTxJson)
    val kTxBlob            = findRequiredStringField("tx_blob", result) // This has SigningPubKey in it?

    // val txnsigFromRq: String = getOrLog(RippleLocalAPI.sign(tx_jsonRq, seed, keyType))
    val key                                           = getOrLog(RippleLocalAPI.packSigningKeyFromB58(seed, keyType))
    val txnsigFromRs: TxnSignature = getOrLog(RippleLocalAPI.sign(kTxJson, key))
    val cTxBlob: BinarySerializer.NestedEncodedValues = RippleCodecAPI.binarySerialize(kTxJson).right.value

    logger.info(s"=====\nGot/Excpted TxBlob: \n ${cTxBlob.toHex} \n $kTxBlob\n\n")
    cTxBlob.toHex shouldEqual kTxBlob

    txnsigFromRs.hex shouldEqual kTxSig
    // txnsigFromRq shouldEqual txnsigFromRs   // If not then probably a field not populated (like SigningPubKey!)s

    ()
  }

  test("All SECP") {
    val secpData: List[(JsonObject, JsonObject)] =
      loadRequestResponses("/test/myTestData/keysAndTxn/secp256k1_txn.json")

    logger.info(s"Testing ${secpData.length} cases")
    secpData.foreach(v ⇒ testJustSigning(v._1, v._2))
  }

  test("All ED25519") {

    val edData: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/keysAndTxn/ed25519_txn.json")
    logger.info(s"Testing ${edData.length} cases")
    edData.foreach(v ⇒ testJustSigning(v._1, v._2))
  }

  test("All Txn") {
    val edData: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/txnscenarios/all_txns.json")
    logger.info(s"Testing ${edData.length} cases")
    edData.drop(1).foreach(v ⇒ testJustSigning(v._1, v._2))
  }

  test("ed master") {
    val edData = loadRequestResponses("/test/myTestData/txnscenarios/ed25519_none_txns.json")
    logger.info(s"Testing ${edData.length} cases")
    edData.foreach(v ⇒ testJustSigning(v._1, v._2))
  }

  test("ed ed") {
    val edData = loadRequestResponses("/test/myTestData/txnscenarios/ed25519_regular_ed25519_txns.json")
    logger.info(s"Testing ${edData.length} cases")
    edData.foreach(v ⇒ testJustSigning(v._1, v._2))
  }

  test("ed secp") {
    val edData = loadRequestResponses("/test/myTestData/txnscenarios/ed25519_regular_secp256k1_txns.json")
    logger.info(s"Testing ${edData.length} cases")
    edData.foreach(v ⇒ testJustSigning(v._1, v._2))
  }

}
