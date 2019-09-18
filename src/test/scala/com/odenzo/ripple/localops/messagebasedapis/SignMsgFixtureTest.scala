package com.odenzo.ripple.localops.messagebasedapis

import io.circe.{Json, JsonObject}
import io.circe.optics.JsonPath
import io.circe.syntax._

import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.EncodedSTObject
import com.odenzo.ripple.bincodec.testkit.JsonReqRes
import com.odenzo.ripple.localops.impl.BinCodecProxy
import com.odenzo.ripple.localops.testkit.{FixtureUtils, OTestSpec}
import com.odenzo.ripple.localops.{LOpJsonErr, LocalOpsError, MessageBasedAPI}

class SignMsgFixtureTest extends OTestSpec with FixtureUtils {

  private lazy val ed: Either[LocalOpsError, List[JsonReqRes]] = loadRqRsResource(
    "/test/myTestData/signrqrs/ed25519_txn.json"
  )

  private lazy val secp: Either[LocalOpsError, List[JsonReqRes]] = loadRqRsResource(
    "/test/myTestData/signrqrs/secp256k1_txn.json"
  )

  test("All") {
    val rr: List[JsonReqRes] = getOrLog(ed) ::: getOrLog(secp)
    rr.foreach(testAllGood)

  }

  /** Goes through all the possible test cases we can mimic and get the same result. */
  def testAllGood(rr: JsonReqRes): Unit = {
    // Should match except
    val response: Json = MessageBasedAPI.sign(rr.rq)
    logger.debug(s"Response:\n${response.asJson.spaces4}")
    response shouldEqual removeDeprecated(rr.rs)
  }

  def runOne(rr: JsonReqRes): Unit = {
    val tx_json: Json                                  = getOrLog(lensGetOpt(JsonPath.root.result.tx_json.json)(rr.rs))
    val rsDump: Either[LocalOpsError, EncodedSTObject] = BinCodecProxy.binarySerialize(tx_json)
    scribe.debug(s"Binary Serialized Rs Result: ${rsDump.show}")

    // val txnsigFromRq: String = getOrLog(RippleLocalAPI.sign(tx_jsonRq, seed, keyType))

    val signRs           = MessageBasedAPI.sign(rr.rq)
    val kResult          = findRequiredField("result", rr.rs)
    val signResult       = findRequiredField("result", signRs)
    val signResultTxJson = findRequiredField("tx_json", signResult)
    val kTxnSignature    = findRequiredField("TxnSignature", signResultTxJson)

    scribe.info(s"Calc Result: \n${signRs.asJson.spaces4}")

    findFieldAsString("status", signRs) shouldEqual findFieldAsString("status", rr.rs)

    val blob: String         = getOrLog(findFieldAsString("tx_blob", signResult))
    val expectedBlob: String = getOrLog(findFieldAsString("tx_blob", kResult))
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
