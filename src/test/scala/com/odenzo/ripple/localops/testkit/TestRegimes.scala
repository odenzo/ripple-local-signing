package com.odenzo.ripple.localops.testkit

import io.circe.Json
import io.circe.syntax._

import cats._
import cats.data._
import cats.implicits._
import scribe.Logging

trait TestRegimes extends OTestUtils with Logging with CodecTestCreators {

  /** Start to make a standard detailed one as a utility. Exercise all top level functionality
    * even though a bit redundant */
  def testSignRqRs(rr: JsonReqRes): Either[Throwable, Unit] = {
    logger.debug(s"Testing A Signed Request Response:\n ${rr.show}")

    for {
      txjson <- findTxJsonInReply(rr.rs.asJson)
      _      <- checkTxBlob(rr.rs.asJson)
      _      <- checkHash(txjson.asJson)
    } yield ()
  }

  /** From  */
  def testLedgerTxn(txn: Json): Either[Throwable, Json] = {
    logger.debug(s"Testing A Single Ledger Txn\n ${txn.spaces4}")
    // No TxBlob but has a hash
    for {
      hash <- checkHash(txn)
    } yield hash
  }

}

object TestRegimes extends TestRegimes
