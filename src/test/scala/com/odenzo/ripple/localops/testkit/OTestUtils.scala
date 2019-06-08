package com.odenzo.ripple.localops.testkit

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import io.circe.syntax._

import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import com.odenzo.ripple.localops.impl.utils.JsonUtils
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.impl.utils.caterrors.ErrorHandling.ErrorOr

trait OTestUtils extends JsonUtils {

  /** Removes the deprecated field in result iff its present*/
  def removeDeprecated(rs: JsonObject): ErrorOr[JsonObject] = {
    rs.asJson.hcursor.downField("result").downField("deprecated").delete.top match {
      case None       => rs.asRight
      case Some(json) => json2jsonObject(json)
    }
  }

  def findTxJsonInResult(rs: JsonObject): Either[AppError, JsonObject] = {
    findObjectField("result", rs).flatMap(findObjectField("tx_json", _))
  }

  def findTxBlobInResult(rs: JsonObject): Either[AppError, String] = {
    findTxJsonInResult(rs).flatMap(findStringField("tx_blob", _))
  }

  /** Check to see in the tx_blob field in result object of param is equal.
    * Spits out detailed difference to logs if not.
    */
  def checkTxBlobs(a: JsonObject, b: JsonObject): Either[Throwable, Boolean] = {
    import com.odenzo.ripple.bincodec.syntax.debugging._
    for {
      exTxBlob <- findTxBlobInResult(a)
      cTxBlob  <- findTxBlobInResult(b)
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
  def checkResults(got: JsonObject, expected: JsonObject): Either[Throwable, Boolean] = {

    checkTxBlobs(got, expected)

    /**
      * Now we can check the whole object, including items outside tx_json
      * Checked regardless of if tx_blobs matched
      */
    for {
      gotClean      <- removeDeprecated(got)
      expectedClean <- removeDeprecated(expected)
      matched = gotClean === expectedClean
    } yield matched
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
