package com.odenzo.ripple.localops.testkit

import io.circe.optics.JsonPath
import io.circe.{Json, JsonObject}
import io.circe.syntax._

import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import com.odenzo.ripple.localops.ErrorHandling.ErrorOr
import com.odenzo.ripple.localops.{LOpJsonErr, LocalOpsError}
import com.odenzo.ripple.localops.impl.utils.JsonUtils

trait OTestUtils extends JsonUtils {

  /**
    *
    * @param v      Vaule to set, None to remove the field
    * @param json   Json containing 'id' field
    * @return
    */
  def setId(v: Option[Json], json: Json): Json = {
    JsonPath.root.at("id").set(None)(json)
  }

  /** Removes the deprecated field in result iff its present*/
  def removeDeprecated(rs: Json): Json = {
    rs.hcursor.downField("result").downField("deprecated").delete.top match {
      case None       => rs
      case Some(json) => json
    }
  }

  def findResultInReply(rs: Json): Either[LOpJsonErr, Json] = {
    findField("result", rs)
  }

  /** At result get the enclosed tx_json */
  def findTxJsonInResult(rs: Json): Either[LocalOpsError, Json] = {
    findResultInReply(rs).flatMap(findField("tx_json", _))
  }

  /** At result field, get the TxBlob enclosed */
  def findTxBlobInReply(rs: Json): Either[LocalOpsError, String] = {
    findResultInReply(rs).flatMap(findFieldAsString("tx_blob", _))
  }

  /** Check to see in the tx_blob field in result object of param is equal.
    * Spits out detailed difference to logs if not.
    */
  def checkTxBlobs(a: Json, b: Json): Either[Throwable, Boolean] = {
    for {
      exTxBlob <- findTxBlobInReply(a)
      cTxBlob  <- findTxBlobInReply(b)
      exEnc    <- TxBlobBuster.bust(exTxBlob)
      gotEnv   <- TxBlobBuster.bust(cTxBlob)
      matched = (exTxBlob === cTxBlob) match {
        case true => true
        case false => {
          logger.warn(s"Got  vs Expected Blob Len: ${cTxBlob.length} and Got ${exTxBlob.length}")
          logger.info(s"Got vs Expected Blob \n $cTxBlob \n $exTxBlob")
          logger.info(s"TxBlob Got    Field: ${gotEnv.show}")
          logger.info(s"TxBlob Target Field: ${exEnc.show}")
          false
        }
      }

    } yield matched
  }

  /**
    * Compare two result object, ensuring txblob matches.
    * This will igore the deprecated field (and maybe the hash?)
    * @param got      SignRs full that we calculated
    * @param expected SignRs full from trace that is should equal
    *
    * @return
    */
  def checkResults(got: Json, expected: Json): Either[Throwable, Boolean] = {

    val detailedTxBlob: Either[Throwable, Boolean] = checkTxBlobs(got, expected)

    /*
     * Now we can check the whole object, including items outside tx_json
     * Checked regardless of if tx_blobs matched
     */
    val exact = removeDeprecated(got) == removeDeprecated(expected)
    detailedTxBlob.map(t => t && exact)

  }

  /** All fields in request are strings */
  def req2field(fix: JsonObject, fieldName: String): Option[String] = {
    fix("Request").flatMap(_.asObject).flatMap(v => v(fieldName)).flatMap(_.asString)
  }

  /** All fields in respone are strings */
  def res2field(fix: JsonObject, fieldName: String): Option[String] = {
    val result: Option[JsonObject] = fix("Response").flatMap(_.asObject).flatMap(v => v("result")).flatMap(_.asObject)
    val field                      = result.flatMap(v => v(fieldName))
    field.flatMap(_.asString)
  }
}
object OTestUtils extends OTestUtils
