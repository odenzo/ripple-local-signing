package com.odenzo.ripple.localops.fixtures

import io.circe.syntax._
import io.circe.{Decoder, Json, JsonObject}

import cats._
import cats.data._
import cats.implicits._
import scribe.Level

import com.odenzo.ripple.localops.impl.utils.CirceUtils
import com.odenzo.ripple.localops.testkit.OTestSpec
import com.odenzo.ripple.localops.{LocalOpsError, RippleLocalAPI}

/** This does tests on Transactions sourced from the ledger, aka LedgerTxn.
  * Now we keep the metadata, but really no use for it.
  **/
class LedgerTxnFixture extends OTestSpec {

  /*
   * A series of transactions have been extracted from a ledger using Ledger(Rq/Rs).
   * Load from file and try and verify.
   *
   */

  test("Verify") {
    super.setTestLogLevel(Level.Debug)
    // Should just be listing all in directory
    // We should be able to verify them all. And also compute the correct hash
    val done = for {
      txjsons <- prepare("/test/myTestData/ledgertxn/ledger_txns.json")
      ok <- txjsons.traverseWithIndexM {
        case (jobs, indx) =>
          logger.info(s"Verifying: #$indx ${jobs.asJson.spaces4}")
          RippleLocalAPI.verify(jobs.asJson) match {
            case Right(true)   => true.asRight
            case Right(false)  => LocalOpsError("Verification was False", jobs.asJson).asLeft
            case err @ Left(_) => err
          }

      }

    } yield ok
    val verified: List[Boolean] = getOrLog(done)
    logger.info(s"$verified")
    verified.forall((v: Boolean) => v) shouldBe true
  }

  def prepare(resourcePath: String): Either[LocalOpsError, List[Json]] = {
    for {
      json   <- loadJsonResource(resourcePath)
      nonull <- CirceUtils.pruneNullFields(json)
      jobjs  <- decode(nonull, Decoder[List[JsonObject]])
      cleaned = jobjs.map(_.remove("metaData").asJson)

    } yield cleaned
  }
}
