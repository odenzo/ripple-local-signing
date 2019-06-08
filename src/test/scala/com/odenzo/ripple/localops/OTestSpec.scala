package com.odenzo.ripple.localops

import java.net.URL
import java.security.{Provider, Security}
import scala.io.{BufferedSource, Source}

import com.typesafe.scalalogging.{Logger, StrictLogging}
import io.circe.{Decoder, Json, JsonObject}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.{EitherValues, Matchers}

import com.odenzo.ripple.localops.utils.CirceUtils
import com.odenzo.ripple.localops.utils.caterrors.AppError.dump
import com.odenzo.ripple.localops.utils.caterrors.CatsTransformers.ErrorOr
import com.odenzo.ripple.localops.utils.caterrors.{AppError, AppException}

trait OTestSpec extends StrictLogging with Matchers with EitherValues {

  Security.addProvider(new BouncyCastleProvider)
  val provider: Provider = Security.getProvider("BC")

  /**
    * This will load from resources/test/fixtures/...
    * Most of those were stolen from Ripple Javascript.
    *
    * @param in JSON File Name as input to a test fixture
    * @param out JSON File Name matching the desired result
    */
  def loadFixture(in: String, out: String): ErrorOr[(Json, Json)] = {

    for {
      inJson <- loadJsonResource(s"/test/fixtures/$in")
      okJson ← loadJsonResource(s"/test/fixtures/$out")
    } yield (inJson, okJson)

  }

  def loadJsonResource(path: String): Either[AppError, Json] = {
    AppException.wrap(s"Getting Resource $path") {
      val resource: URL          = getClass.getResource(path)
      val source: BufferedSource = Source.fromURL(resource)
      val data: String           = source.getLines().mkString("\n")
      CirceUtils.parseAsJson(data)
    }
  }

  def getOrLog[T](ee: ErrorOr[T], msg: String = "Error: ", loggger: Logger = logger): T = {
    if (ee.isLeft) {
      dump(ee) match {
        case None       ⇒ loggger.debug("No Errors Found")
        case Some(emsg) ⇒ loggger.error(s"$msg\t=> $emsg ")
      }
      assert(false, s"Auto Test of $msg")

    }
    ee.right.value
  }

}

/**
  * Common to have object with binary and json in test files.
  * @param binary
  * @param json
  */
case class TestFixData(json: JsonObject, binary: String)

object TestFixData {

  import io.circe.generic.semiauto.deriveDecoder

  implicit val decoder: Decoder[TestFixData] = deriveDecoder[TestFixData]
}
