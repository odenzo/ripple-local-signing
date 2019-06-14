package com.odenzo.ripple.localops.myfixtures

import cats._
import cats.data._
import io.circe.JsonObject
import io.circe.syntax._
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.utils.caterrors.AppError
import com.odenzo.ripple.localops.utils.{ByteUtils, FixtureUtils}
import com.odenzo.ripple.localops.{OTestSpec, RippleLocalAPI}

/** This test is designed check the secret to pubsigning key conversion
  * Lots of different types of secrets
  * */
class VerificationFixture$Test extends FunSuite with OTestSpec with ByteUtils with FixtureUtils {

  /** See if we can get the correct txnscenarios Public Key and Hash to start */
  def testOne(rs: JsonObject) = {
    logger.debug(s"Response: ${rs.asJson.spaces4}")

    val result                              = findRequiredObject("result", rs)
    val tx_json: JsonObject                 = findRequiredObject("tx_json", result)
    val verified: Either[AppError, Boolean] = RippleLocalAPI.verify(tx_json)
    val ok                                  = getOrLog(verified)
    ok shouldEqual true
  }

  test("ALL SECP") {
    val txnFixt: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/keysAndTxn/secp256k1_txn.json")
    txnFixt.foreach(v ⇒ testOne(v._2))
  }

  test("ALL ED25519") {
    val txnFixt: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/keysAndTxn/ed25519_txn.json")
    txnFixt.foreach(v ⇒ testOne(v._2))
  }

  test("mixed_txn") {
    val txnFixt: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/txnscenarios/all_txns.json")
    txnFixt.foreach(v ⇒ testOne(v._2))
  }
}
