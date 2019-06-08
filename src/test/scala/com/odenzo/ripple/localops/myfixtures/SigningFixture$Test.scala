package com.odenzo.ripple.localops.myfixtures

import io.circe.JsonObject
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.utils.caterrors.AppError
import com.odenzo.ripple.localops.utils.{ByteUtils, FixtureUtils, JsonUtils}
import com.odenzo.ripple.localops.{OTestSpec, RippleLocalAPI}

/**
  *  Goes through some server signed txn and results and does local signing to check correct
  *  TODO: TxnSignature correct, add code to make and check TxBlob for submission
  */
class SigningFixture$Test extends FunSuite with OTestSpec with ByteUtils with FixtureUtils with JsonUtils {

  val txnfixture: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/Signing/secp256k1_txn.json")

  def testJustSigning(rq: JsonObject, rs: JsonObject): Unit = {
    // This assumes all required fields are filled in.

    val tx_jsonRq = findRequiredObject("tx_json",rq)
    val seed: String = findRequiredStringField("seed", rq)
    val keyType: String = findRequiredStringField("key_type", rq)


     val txnsig: Either[AppError, String] = RippleLocalAPI.sign(tx_jsonRq, seed, keyType)


    val result: JsonObject = findRequiredObject("result", rs)
    val kTxJson = findRequiredObject("tx_json", result)
    val kTxSig: String = findRequiredStringField("TxnSignature", kTxJson)

    txnsig.right.value shouldEqual kTxSig

    ()
  }

  // Need some complete messages
  test("First") {
    txnfixture.take(1).foreach(v ⇒ testJustSigning(v._1, v._2))
  }

  test("All") {
    logger.info(s"Testing ${txnfixture.length} cases")
    txnfixture.foreach(v ⇒ testJustSigning(v._1, v._2))
  }

}
