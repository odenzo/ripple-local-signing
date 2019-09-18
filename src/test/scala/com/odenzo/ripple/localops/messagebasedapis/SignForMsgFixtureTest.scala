package com.odenzo.ripple.localops.messagebasedapis

import io.circe.Json
import io.circe.syntax._

import cats._
import cats.data._
import scribe.Level

import com.odenzo.ripple.bincodec.testkit.{FKP, JsonReqRes}
import com.odenzo.ripple.localops.impl.utils.ScribeLogUtils
import com.odenzo.ripple.localops.testkit.{FixtureUtils, OTestSpec}
import com.odenzo.ripple.localops.{LocalOpsError, MessageBasedAPI}

class SignForMsgFixtureTest extends OTestSpec with FixtureUtils {

  private val baseResource = "/test/myTestData/multisign"

  customLogLevel = Level.Debug
  // The keys involved in multsigning
  private lazy val keys: Either[LocalOpsError, List[FKP]] = loadKeysResource(
    s"$baseResource/secp_multisigners.json"
  )

  private lazy val secpTxn: Either[LocalOpsError, List[JsonReqRes]] = loadRqRsResource(
    s"$baseResource/secp_multisigns_txn.json"
  )

  test("All Single Sign") {
    ScribeLogUtils.mutePackages(List("com.odenzo.ripple.bincodec", "com.odenzo.ripple.localops.impl.crypto"))
    val rr: List[JsonReqRes] = getOrLog(secpTxn)
    rr.foreach(testAllGood)

  }

  test("Combine Three Singles") {}

  /** Goes through all the possible test cases we can mimic and get the same result. */
  def testAllGood(rr: JsonReqRes): Unit = {
    // Should match except ?  The rq json is full request
    val response: Json = MessageBasedAPI.signFor(rr.rq)
    logger.debug(s"Fixture Request :\n${rr.rq.spaces4}")
    logger.debug(s"Fixture Response:\n${rr.rs.spaces4}")

    logger.debug(s"Response:\n${response.spaces4}")
    val cleanRs = removeDeprecated(rr.rs)
    response shouldEqual removeDeprecated(rr.rs)
  }

}
