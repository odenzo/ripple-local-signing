package com.odenzo.ripple.localops.testkit

import java.security.{Provider, Security}

import io.circe.{Decoder, Json, JsonObject}

import cats._
import cats.data._
import cats.implicits._
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.{BeforeAndAfterAll, EitherValues, FunSuiteLike, Matchers}
import scribe.{Level, Logger, Logging, Priority}

trait OTestSpec
    extends FunSuiteLike
    with OTestUtils
    with OTestLogging
    with FixtureUtils
    with Matchers
    with EitherValues {

  Security.addProvider(new BouncyCastleProvider)
  val provider: Provider = Security.getProvider("BC")

  def findRequiredStringField(name: String, jobj: Json): String = {
    getOrLog(findField(name, jobj).flatMap(json2string))
  }

  /**
    *
    * @return Json of field or logging of error and assertion failure
    */
  def findRequiredField(name: String, json: Json): Json = {
    getOrLog(findField(name, json))
  }

  def findRequiredObject(name: String, json: Json): JsonObject = {
    val asObj = findFieldAsObject(name, json)
    getOrLog(asObj)
  }

  def getOrLog[T](ee: Either[Throwable, T], msg: String = "Error: ", myLog: Logger = logger): T = {
    import com.odenzo.ripple.localops.LOpException._
    ee match {
      case Right(v) => v
      case Left(e) =>
        logger.error(s"${e.show}")
        fail(s"getOrLog error ${e.getMessage}")
    }
  }

}

object OTestSpec extends Logging {}

/**
  * Common to have object with binary and json in test files.
  *
  * @param binary
  * @param json
  */
case class TestFixData(json: JsonObject, binary: String)

object TestFixData {

  import io.circe.generic.semiauto.deriveDecoder

  implicit val decoder: Decoder[TestFixData] = deriveDecoder[TestFixData]
}
