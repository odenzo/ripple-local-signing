package com.odenzo.ripple.localops.testkit

import java.security.{Provider, Security}

import io.circe.{Decoder, Json, JsonObject}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.{BeforeAndAfterAll, EitherValues, FunSuiteLike, Matchers}
import scribe.{Level, Logger, Logging, Priority}

import com.odenzo.ripple.localops.impl.utils.ScribeLogUtils
import com.odenzo.ripple.localops.impl.utils.caterrors.AppErrorUtils

trait OTestSpec
    extends FunSuiteLike
    with OTestUtils
    with FixtureUtils
    with OTestLogging
    with Matchers
    with EitherValues
    with BeforeAndAfterAll {

  // Well, it seems that each test is getting built/instanciated before runing.

  Security.addProvider(new BouncyCastleProvider)
  val provider: Provider = Security.getProvider("BC")

  protected var customLogLevel: Level = Level.Warn

  override def beforeAll(): Unit = {
    setTestLogLevel(customLogLevel)
  }

  override def afterAll(): Unit = {
    setTestLogLevel(Level.Warn)
  }

  def findRequiredStringField(name: String, jobj: JsonObject): String = {
    getOrLog(findField(name, jobj).flatMap(json2string))
  }

  /**
    *
    * @return Json of field or logging of error and assertion failure
    */
  def findRequiredField(name: String, json: JsonObject): Json = {
    getOrLog(findField(name, json))
  }

  def findRequiredObject(name: String, jsonObject: JsonObject): JsonObject = {
    val asObj = findObjectField(name, jsonObject)
    getOrLog(asObj)
  }

  def getOrLog[T](ee: Either[Throwable, T], msg: String = "Error: ", myLog: Logger = logger): T = {
    ee match {
      case Right(v) => v
      case Left(e) =>
        AppErrorUtils.showThrowable.show(e)
        fail(s"getOrLog error ${e.getMessage}")
    }
  }

}

object OTestSpec extends Logging {

  // bincodec is still using scribe.Logger
  // localops is using mixing Logging / logger
  logger.warn("Cranking Logging Down To WARN IN OBJECT")

  val x = testLoggingSetup()

  def testLoggingSetup(): Unit = {
    if (!ScribeLogUtils.inCITesting) {
      logger.warn("Think I am in Travis")
      scribe.Logger.root.clearHandlers().clearModifiers().withHandler(minimumLevel = Some(Level.Error)).replace()

      val packagesToMute: List[String] = List(
        "com.odenzo.ripple.bincodec",
        "com.odenzo.ripple.localops"
      )
      val pri = Priority.High // unnecessary since clearing existing modifiers, but handy for future.
      scribe.Logger.root
        .clearModifiers()
        .withModifier(ScribeLogUtils.excludePackageSelction(packagesToMute, Level.Warn, pri))
        .replace()

      Logger.root
        .clearModifiers()
        .withModifier(ScribeLogUtils.excludePackageSelction(packagesToMute, Level.Warn, pri))
        .replace()

    } else {
      logger.warn("Regular Testing")
      val packagesToMute: List[String] = List(
        "com.odenzo.ripple.bincodec",
        "com.odenzo.ripple.localops"
      )
      val pri = Priority.High // unnecessary since clearing existing modifiers, but handy for future.
      scribe.Logger.root
        .clearModifiers()
        .withModifier(ScribeLogUtils.excludePackageSelction(packagesToMute, Level.Warn, pri))
        .replace()

      Logger.root
        .clearModifiers()
        .withModifier(ScribeLogUtils.excludePackageSelction(packagesToMute, Level.Warn, pri))
        .replace()

    }

    scribe.Logger.root.clearHandlers().clearModifiers().withHandler(minimumLevel = Some(Level.Error)).replace()
    ()
  }
}

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
