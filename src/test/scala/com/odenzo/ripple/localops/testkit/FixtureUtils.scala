package com.odenzo.ripple.localops.testkit

import java.net.URL
import scala.io.{BufferedSource, Source}

import cats._
import cats.data._
import cats.effect.{IO, Resource}
import cats.implicits._
import io.circe.syntax._
import io.circe.{Decoder, Json, JsonObject}

import com.odenzo.ripple.bincodec.Decoded
import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.impl.utils.caterrors.ErrorHandling.ErrorOr
import com.odenzo.ripple.localops.impl.utils.{CirceUtils, JsonUtils}

/** Fixture utils that help load or create fixture files.
  * These should not depend on being in ScalaTest context.
  */
trait FixtureUtils extends JsonUtils {

  def loadJsonResource(path: String): Either[AppError, Json] = {
    AppError.wrap(s"Getting Resource $path") {
      val resource: URL          = getClass.getResource(path)
      val source: BufferedSource = Source.fromURL(resource)
      val data: String           = source.getLines().mkString("\n")
      CirceUtils.parseAsJson(data)
    }
  }

  /** Construct a Cats Resource with the JSON parsed from the named Java resource
    *
    * */
  def makeJsonResource(path: String): ErrorOr[Json] = {

    val url: URL = getClass.getResource(path)
    val acquire  = IO(Source.fromURL(url))
    val resource = Resource.fromAutoCloseable(acquire)

    val json = resource.use { src =>
      IO(CirceUtils.parseAsJson(src.mkString))
    }

    json.unsafeRunSync()

  }

  /**
    * Expects resource containing JSON array of objects, each object having a "request" and "response" field.
    *
    * @param resource
    *
    * @return List of JSONObject tuple for each request and response
    */
  def loadRqRsResource(resource: String): Either[AppError, List[JsonReqRes]] = {
    for {
      json <- loadJsonResource(resource)
      objs <- CirceUtils.decode(json, Decoder[List[JsonReqRes]])
    } yield objs
  }

  /**
    * @return List of FullKeyPairs from array of objects containing master and possible regular key AccountKeys
    */
  def loadKeysResource(resource: String): Either[AppError, List[FKP]] = {
    for {
      json <- loadJsonResource(resource)
      objs <- CirceUtils.decode(json, Decoder[List[FKP]])
    } yield objs
  }

  /** Loads file of standard request response format for wallet propose and
    * decodes the result into AccountKeys. Keeps rq in raw format for no good reason.
    *
    * @param resource
    *
    * @return
    */
  def loadWalletRqRs(resource: String): Either[AppError, List[(JsonObject, AccountKeys)]] = {

    for {
      rr <- loadRqRsResource(resource)
      rqKeys <- rr.traverse { v =>
        CirceUtils.decode(v.rs, AccountKeys.decoder).tupleLeft(v.rq)
      }
    } yield rqKeys
  }

  def loadFixtureSubset(resource: String, start: Int, number: Int = 1): Either[AppError, List[(JsonReqRes, Int)]] = {
    val result: Either[AppError, List[(JsonReqRes, Int)]] = for {
      zip <- loadFixture(resource)
      _      = logger.info(s"Limiting to Range $start -> ${start + number - 1}")
      subset = zip.slice(start, start + number)
    } yield subset
    result

  }

  def loadFixture(resource: String): Either[AppError, List[(JsonReqRes, Int)]] = {
    val result: Either[AppError, List[(JsonReqRes, Int)]] = for {
      data <- loadRqRsResource(resource)
      _   = logger.debug(s"Rq Rs Resource $resource has ${data.size} elements.")
      zip = data.zipWithIndex
    } yield zip
    result
  }

  /** Executes the functions on the given list with some handy wrappers.
    * Stops execution on first failure
    * This should only be called from with a ScalaTest.
    */
  def executeFixture[T](sample: List[(JsonReqRes, Int)])(fn: (JsonReqRes => T)): List[T] = {
    sample.map {
      case (rr, indx) =>
        scribe.info(s"\n\n\n\n===================== INDEX $indx =============")
        scribe.info(s"Req:\n ${rr.rq.asJson.spaces4}")
        scribe.info(s"Res:\n ${rr.rs.asJson.spaces4}")
        fn(rr)
    }

  }

  /**
    *  getorLog this, as it will fail on bad loading of fixture
    * @param resource
    * @param fn
    * @return
    */
  def loadAndExecuteFixture[T](resource: String)(fn: (JsonReqRes => T)): Either[AppError, List[T]] = {
    loadFixture(resource).map((v: List[(JsonReqRes, Int)]) => executeFixture(v)(fn))
  }

}
