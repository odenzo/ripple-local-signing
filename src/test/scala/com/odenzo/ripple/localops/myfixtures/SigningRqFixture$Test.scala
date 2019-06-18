package com.odenzo.ripple.localops.myfixtures

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import io.circe.syntax._

import org.scalatest.FunSuite

import com.odenzo.ripple.localops.utils.{ByteUtils, FixtureUtils, JsonUtils}
import com.odenzo.ripple.localops.{OTestSpec, RippleLocalAPI}

import com.odenzo.ripple.bincodec.serializing.DebuggingShows._
/**
  *  Goes through some server signed txn and results and does local signing udsing the SignRq / SignRs
  */
class SigningRqFixture$Test extends FunSuite with OTestSpec with ByteUtils with FixtureUtils with JsonUtils {

  def runOne(rq: JsonObject, rs: JsonObject): Unit = {

    val rsResultTx = findRequiredObject("tx_json", findRequiredObject("result", rs))
    val rsDump = RippleLocalAPI.binarySerialize(rsResultTx)
    logger.info(s"Binary Serialized Rs Result: ${rsDump.show}")


    // val txnsigFromRq: String = getOrLog(RippleLocalAPI.sign(tx_jsonRq, seed, keyType))

    val signRs: JsonObject = RippleLocalAPI.sign(rq)
    val kResult            = findRequiredObject("result", rs)
    val signResult         = findRequiredObject("result", signRs)

    logger.debug(s"Fix SignRq: \n${rq.asJson.spaces4}")
    logger.debug(s"Fix SignRs: \n${rs.asJson.spaces4}")
    logger.info(s"Calc Result: \n${signRs.asJson.spaces4}")

    findStringField("status", signRs) shouldEqual findStringField("status", rs)
    findStringField("tx_blob", signResult) shouldEqual findStringField("tx_blob", kResult)
    ()
  }

  test("AllGood Txn") {
    val data: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/txnscenarios/all_txns.json")
    logger.info(s"Testing ${data.length} cases")
    data.zipWithIndex.foreach{ case ((rq,rs),indx) ⇒
      logger.info(s"\n\n\n\n===================== INDEX $indx =============")
      runOne(rq, rs)
    }
  }


  test("Some of all_txn"){
    val data: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/txnscenarios/all_txns.json")
    logger.info(s"Testing ${data.length} cases")
    data.zipWithIndex.drop(4).take(1).foreach{ case ((rq, rs), indx) ⇒
      logger.info(s"\n\n\n\n===================== INDEX $indx =============")
      runOne(rq, rs)
    }
  }

}
