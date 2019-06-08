package com.odenzo.ripple.localops.messagebasedapis

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import io.circe.syntax._

import com.odenzo.ripple.bincodec.EncodedSTObject
import com.odenzo.ripple.localops.MessageBasedAPI
import com.odenzo.ripple.localops.impl.BinCodecProxy
import com.odenzo.ripple.localops.testkit.{FixtureUtils, JsonReqRes, OTestSpec}

class SignMsgFixtureTest extends OTestSpec with FixtureUtils {

  import com.odenzo.ripple.bincodec.syntax.debugging._
  import com.odenzo.ripple.localops.impl.utils.caterrors._

  private lazy val ed: Either[AppError, List[JsonReqRes]] = loadRqRsResource(
    "/test/myTestData/keysAndTxn/ed25519_txn.json"
  )

  private lazy val secp: Either[AppError, List[JsonReqRes]] = loadRqRsResource(
    "/test/myTestData/keysAndTxn/secp256k1_txn.json"
  )

  test("All") {
    val rr: List[JsonReqRes] = getOrLog(ed) ::: getOrLog(secp)
    rr.foreach(testAllGood)

  }

  /** Goes through all the possible test cases we can mimic and get the same result. */
  def testAllGood(rr: JsonReqRes): Unit = {
    // Should match except
    val response: JsonObject = MessageBasedAPI.sign(rr.rq)
    logger.debug(s"Response:\n${response.asJson.spaces4}")

    response shouldEqual getOrLog(removeDeprecated(rr.rs))

  }

  def runOne(rr: JsonReqRes): Unit = {
    val rsResultTx                                = findRequiredObject("tx_json", findRequiredObject("result", rr.rs))
    val rsDump: Either[AppError, EncodedSTObject] = BinCodecProxy.binarySerialize(rsResultTx)
    scribe.debug(s"Binary Serialized Rs Result: ${rsDump.show}")

    // val txnsigFromRq: String = getOrLog(RippleLocalAPI.sign(tx_jsonRq, seed, keyType))

    val signRs: JsonObject = MessageBasedAPI.sign(rr.rq)
    val kResult            = findRequiredObject("result", rr.rs)
    val signResult         = findRequiredObject("result", signRs)
    val signResultTxJson   = findRequiredObject("tx_json", signResult)
    val kTxnSignature      = findStringField("TxnSignature", signResultTxJson)

    scribe.info(s"Calc Result: \n${signRs.asJson.spaces4}")

    findStringField("status", signRs) shouldEqual findStringField("status", rr.rs)

    val blob: String         = getOrLog(findStringField("tx_blob", signResult))
    val expectedBlob: String = getOrLog(findStringField("tx_blob", kResult))
    if (blob != expectedBlob) {
      val produced = getOrLog(BinCodecProxy.decodeBlob(blob))
      val pStr     = produced.map(v => v.show).mkString("\n")
      val expected = getOrLog(BinCodecProxy.decodeBlob(expectedBlob))
      val eStr     = expected.map(_.show).mkString("\n")
      scribe.info(s"Produced: \n $pStr")
      scribe.info(s"Expected: \n $eStr")
    }

    blob shouldEqual expectedBlob
    ()
  }

}
