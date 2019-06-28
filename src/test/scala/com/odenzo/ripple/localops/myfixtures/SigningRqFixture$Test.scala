package com.odenzo.ripple.localops.myfixtures

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import io.circe.syntax._
import scribe.{Level, Logging}

import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import com.odenzo.ripple.bincodec.syntax.debugging._
import com.odenzo.ripple.localops.utils.{ByteUtils, FixtureUtils, JsonUtils}
import com.odenzo.ripple.localops.{OTestSpec, RippleLocalAPI}

/**
  *  Goes through some server signed txn and results and does local signing udsing the SignRq / SignRs
  */
class SigningRqFixture$Test extends OTestSpec with ByteUtils with FixtureUtils with JsonUtils  {



  
  def runOne(rq: JsonObject, rs: JsonObject): Unit = {

    val rsResultTx = findRequiredObject("tx_json", findRequiredObject("result", rs))
    val rsDump     = RippleLocalAPI.binarySerialize(rsResultTx)
    scribe.info(s"Binary Serialized Rs Result: ${rsDump.show}")

    // val txnsigFromRq: String = getOrLog(RippleLocalAPI.sign(tx_jsonRq, seed, keyType))

    val signRs: JsonObject = RippleLocalAPI.sign(rq)
    val kResult            = findRequiredObject("result", rs)
    val signResult         = findRequiredObject("result", signRs)
    val signResultTxJson   = findRequiredObject("tx_json", signResult)
    val kTxnSignature      = findStringField("TxnSignature", signResultTxJson)
    scribe.debug(s"Fix SignRq: \n${rq.asJson.spaces4}")
    scribe.debug(s"Fix SignRs: \n${rs.asJson.spaces4}")
    scribe.info(s"Calc Result: \n${signRs.asJson.spaces4}")

    findStringField("status", signRs) shouldEqual findStringField("status", rs)

    val blob: String         = getOrLog(findStringField("tx_blob", signResult))
    val expectedBlob: String = getOrLog(findStringField("tx_blob", kResult))
    if (blob != expectedBlob) {
      val produced = TxBlobBuster.bust(blob).right.value
      val pStr     = produced.map(v ⇒ v.show).mkString("\n")
      val expected = TxBlobBuster.bust(expectedBlob).right.value
      val eStr     = expected.map(_.show).mkString("\n")
      scribe.info(s"Produced: \n $pStr")
      scribe.info(s"Expected: \n $eStr")
    }

    blob shouldEqual expectedBlob
    ()
  }

  test("AllGood Txn") {
    val data: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/txnscenarios/all_txns.json")
    logger.info(s"Testing ${data.length} cases")
    data.zipWithIndex.foreach {
      case ((rq, rs), indx) ⇒
        // Setting Global Levels...I am using global logger everywhere
        // This will override other settings,
        //        scribe.Logger.root.clearHandlers().clearModifiers().withHandler(minimumLevel = Some(Level.Debug)).replace()

        scribe.info(s"\n\n\n\n===================== INDEX $indx =============")
        runOne(rq, rs)
    }
  }

  test("Some of all_txn") {
    val data: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/txnscenarios/all_txns.json")


    logger.info(s"Testing ${data.length} cases")
    data.zipWithIndex.drop(54).take(1).foreach {
      case ((rq, rs), indx) ⇒
        scribe.info(s"\n\n\n\n===================== INDEX $indx =============")
        scribe.info(s"Req:\n ${rq.asJson.spaces4}")
        scribe.info(s"Res:\n ${rs.asJson.spaces4}")

        runOne(rq, rs)
    }
  }

}
