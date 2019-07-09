package com.odenzo.ripple.localops.testkit

import java.net.URL
import java.security.{Provider, Security}
import scala.io.{BufferedSource, Source}

import io.circe.{Decoder, Json, JsonObject}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.{EitherValues, FunSuiteLike, Matchers}
import scribe.{Level, Logger, Logging, Priority}

import com.odenzo.ripple.bincodec
import com.odenzo.ripple.bincodec.LoggingConfig
import com.odenzo.ripple.localops.utils.CirceUtils
import com.odenzo.ripple.localops.utils.caterrors.AppError.dump
import com.odenzo.ripple.localops.utils.caterrors.CatsTransformers.ErrorOr
import com.odenzo.ripple.localops.utils.caterrors.{AppError, AppException}

trait OTestSpec extends FunSuiteLike with Matchers with EitherValues with OTestLogging {

  // Well, it seems that each test is getting built/instanciated before runing.

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

  def getOrLog[T](ee: Either[AppError, T], msg: String = "Error: ", myLog: Logger = logger): T = {
    if (ee.isLeft) {
      dump(ee) match {
        case None       ⇒ myLog.debug("No Errors Found")
        case Some(emsg) ⇒ myLog.error(s"$msg\t=> $emsg ")
      }
      assert(false, s"Auto Test of $msg")

    }
    ee.right.value
  }

}

object OTestSpec extends Logging {

  // bincodec is still using scribe.Logger
  // localops is using mixing Logging / logger
  logger.warn("Cranking Logging Down To WARN IN OBJECT")

  val x = testLoggingSetup()

  def testLoggingSetup(): Unit = {
    if (!bincodec.inCI) {
      logger.warn("Think I am in Travis")
      scribe.Logger.root.clearHandlers().clearModifiers().withHandler(minimumLevel = Some(Level.Error)).replace()

      val packagesToMute: List[String] = List(
        "com.odenzo.ripple.bincodec",
        "com.odenzo.ripple.localops",
      )
      val pri = Priority.High // unnecessary since clearing existing modifiers, but handy for future.
      scribe.Logger.root
      .clearModifiers()
      .withModifier(LoggingConfig.excludePackageSelction(packagesToMute, Level.Warn, pri))
      .replace()

      Logger.root
      .clearModifiers()
      .withModifier(LoggingConfig.excludePackageSelction(packagesToMute, Level.Warn, pri))
      .replace()

    } else {
      logger.warn("Regular Testing")
      val packagesToMute: List[String] = List(
        "com.odenzo.ripple.bincodec",
        "com.odenzo.ripple.localops",
      )
      val pri = Priority.High // unnecessary since clearing existing modifiers, but handy for future.
      scribe.Logger.root
        .clearModifiers()
        .withModifier(LoggingConfig.excludePackageSelction(packagesToMute, Level.Warn, pri))
        .replace()

      Logger.root
      .clearModifiers()
      .withModifier(LoggingConfig.excludePackageSelction(packagesToMute, Level.Warn, pri))
      .replace()

    }

    Logger.root.orphan() // Fully detach from console output
    Logger.logger.orphan()
    scribe.Logger.root.clearHandlers().clearModifiers().withHandler(minimumLevel = Some(Level.Error)).replace()
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
