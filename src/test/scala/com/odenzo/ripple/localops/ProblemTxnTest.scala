package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import scribe.Level

import com.odenzo.ripple.bincodec.testkit.{JsonReqRes, TestRegimes}
import com.odenzo.ripple.localops.testkit.OTestSpec

/** I throw any problem transactions in problems.json for exmination and fixes.
  * A few possible test case categories:
  * - SignRq/Rs where I know the key. Should fully emulate, calculte sign and pub signing key and TxBlob
  * - SignRs/Rs w/o key -- can check the TxBlob and verify the signature, which also exercises serializeForSigning
  * - A Transaction Pulled from a ledger (via TxRq or subscription etc)
  * cases, a Rerqust/Response sign in which I
  * do not know the signing
  * */
class ProblemTxnTest extends OTestSpec {
  super.setTestLogLevel(Level.Debug)

  test("Troublesome") {
    super.setTestLogLevel(Level.Debug)
    val done = for {
      txjsons <- prepare("/test/myTestData/txnscenarios/problemRqRs.json")
      ok      <- txjsons.traverse(rr => signedRqRsTestRegime(rr))
    } yield ok

    getOrLog(done)
  }

  /** Start to make a standard detailed one as a utility */
  def signedRqRsTestRegime(rr: JsonReqRes): Either[Throwable, Unit] = {
    logger.debug(s"Testing A Signed Request Response:\n ${rr.show}")

    for {
      _      <- TestRegimes.testSignRqRs(rr) // TestKit from BinCodec -- most errors in there, so dumps diagnostics
      txjson <- findTxJsonInResult(rr.rs)
      _      <- RippleLocalAPI.verify(txjson)
    } yield ()
  }

  def prepare(resourcePath: String): Either[LocalOpsError, List[JsonReqRes]] = {
    for {
      json <- loadRqRsResource(resourcePath)
      // nonull <- CirceUtils.pruneNullFields(json)
      //jobjs  <- decode(nonull, Decoder[List[JsonObject]])
      //cleaned = jobjs.map(_.remove("metaData"))

    } yield json
  }

}
