package com.odenzo.ripple.localops.myfixtures

import cats._
import cats.data._
import cats.implicits._

import io.circe.JsonObject
import io.circe.syntax._
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.utils.caterrors.AppError
import com.odenzo.ripple.localops.utils.{ByteUtils, FixtureUtils, JsonUtils}
import com.odenzo.ripple.localops.{OTestSpec, RippleLocalAPI}

/**
  *  Goes through some server signed txn and results and does local signing to check correct
  *  TODO: TxnSignature correct, add code to make and check TxBlob for submission
  */
class SigningFixture$Test extends FunSuite with OTestSpec with ByteUtils with FixtureUtils with JsonUtils {



  def testJustSigning(rq: JsonObject, rs: JsonObject): Unit = {
    // This assumes all required fields are filled in.
    logger.info(s"Signing ${rq.asJson.spaces4}")
    val tx_jsonRq       = findRequiredObject("tx_json", rq)
    val seed: String    = findRequiredStringField("seed", rq)
    val keyType: String = findRequiredStringField("key_type", rq)

    val txnsig: String = getOrLog(RippleLocalAPI.sign(tx_jsonRq, seed, keyType))

    val result: JsonObject = findRequiredObject("result", rs)
    val kTxJson            = findRequiredObject("tx_json", result)
    val kTxSig: String     = findRequiredStringField("TxnSignature", kTxJson)

    txnsig shouldEqual kTxSig

    ()
  }

  test("All SECP") {
    val secpData: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/keysAndTxn/secp256k1_txn.json")

    logger.info(s"Testing ${secpData.length} cases")
    secpData.foreach(v ⇒ testJustSigning(v._1, v._2))
  }


  test("All ED25519"){

    val edData: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/keysAndTxn/ed25519_txn.json")
    logger.info(s"Testing ${edData.length} cases")
    edData.foreach(v ⇒ testJustSigning(v._1, v._2))
  }

  test("Mixed Txn") {
    val edData: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/txnscenarios/mixed_txn.json")
    logger.info(s"Testing ${edData.length} cases")
    edData.foreach(v ⇒ testJustSigning(v._1, v._2))
  }
}
