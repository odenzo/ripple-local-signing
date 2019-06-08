package com.odenzo.ripple.localops.messagebasedapis

import cats._
import cats.data._
import io.circe.JsonObject
import io.circe.syntax._
import scribe.Level

import com.odenzo.ripple.localops.MessageBasedAPI
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.testkit.{FKP, FixtureUtils, JsonReqRes, OTestSpec}

class SignForMsgFiixtureTest extends OTestSpec with FixtureUtils {

  private val baseResource = "/test/myTestData/multisign"

  customLogLevel = Level.Info
  // The keys involved in multsigning
  private lazy val keys: Either[AppError, List[FKP]] = loadKeysResource(
    s"$baseResource/secp_multisigners.json"
  )

  private lazy val secpTxn: Either[AppError, List[JsonReqRes]] = loadRqRsResource(
    s"$baseResource/secp_multisigns_txn.json"
  )

  test("AddingTest") {
    // Unfortunately we are getting a bit specific, better to make a new fixture scenario.
    val signers: List[FKP]          = getOrLog(keys).drop(2)
    val signForRR: List[JsonReqRes] = getOrLog(secpTxn).take(3)

    // First sign the empty one, which should come back ok with one signer.
    val validRs = signForRR.map { rr =>
      val rs = MessageBasedAPI.signFor(rr.rq.asJson)
      getOrLog(checkResults(rs, rr.rs)) shouldEqual true
      rs
    }
  }

  test("All Single Sign") {
    val rr: List[JsonReqRes] = getOrLog(secpTxn)
    rr.foreach(testAllGood)

  }

  test("Combine Three Singles") {}

  /** Goes through all the possible test cases we can mimic and get the same result. */
  def testAllGood(rr: JsonReqRes): Unit = {
    // Should match except
    val response: JsonObject = MessageBasedAPI.signFor(rr.rq.asJson)
    logger.debug(s"Response:\n${response.asJson.spaces4}")
    val cleanRs = removeDeprecated(rr.rs)
    response shouldEqual getOrLog(removeDeprecated(rr.rs))
  }

}
