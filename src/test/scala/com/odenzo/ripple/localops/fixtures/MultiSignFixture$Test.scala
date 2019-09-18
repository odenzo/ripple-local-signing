package com.odenzo.ripple.localops.fixtures

import io.circe.Json
import io.circe.optics.JsonPath

import cats._
import cats.data._
import cats.implicits._
import monocle.Optional
import scribe.{Level, Logging}

import com.odenzo.ripple.bincodec.EncodedSTObject
import com.odenzo.ripple.bincodec.testkit.JsonReqRes
import com.odenzo.ripple.localops.impl.BinCodecProxy
import com.odenzo.ripple.localops.impl.messagehandlers.SignForMsg
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.testkit.{FixtureUtils, OTestSpec}
import com.odenzo.ripple.localops.{LOpJsonErr, LocalOpsError, MessageBasedAPI, RippleLocalAPI}

/**
  * Tests the RippleLocalAPI and fixtures of SignRq / SignRs
  * for single signed tranasctions. Should work for multisigned too, but need to test.
  *
  * This tests signing and verification
  *
  */
class MultiSignFixture$Test extends OTestSpec with ByteUtils with FixtureUtils with JsonUtils with Logging {

  val lens: Optional[Json, Json] = JsonPath.root.result.tx_json.json
  def testMultiSign(rr: JsonReqRes): Unit = {
    setTestLogLevel(Level.Debug)

    val rs: Json = MessageBasedAPI.signFor(rr.rq)
    rs shouldEqual removeDeprecated(rr.rs)
    for {
      tx_json  <- lensGetOpt(lens)(rs)
      verified <- RippleLocalAPI.verify(tx_json)
      _ = verified shouldEqual true
    } yield ()
    ()
  }

  test("All Txn") {

    val fixs = List(
      "/test/myTestData/multisign/secp_multisigns_txn.json",
      "/test/myTestData/multisign/secp_aggregate_multisign_txn.json"
    )

    fixs.traverse { fix =>
      for {
        data <- loadRqRsResource(fix)
        res = data.map(testMultiSign)
      } yield res
    }
    ()

  }
}
