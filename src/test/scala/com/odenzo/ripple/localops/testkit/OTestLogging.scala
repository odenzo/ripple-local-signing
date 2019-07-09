package com.odenzo.ripple.localops.testkit

import cats.implicits._
import scribe.Level.{Debug, Warn}
import scribe.{Level, Logging, Priority}
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.LoggingConfig
import com.odenzo.ripple.localops.utils.ByteUtils
import com.odenzo.ripple.localops.utils.caterrors.AppError

trait OTestLogging extends Logging {

  // When exactly does this get instanciated? Have to touch it.

  // Need to apply these by package scope for library mode.
  // APply to com.odenzo.ripple.bincodec.*

  scribe.warn("*********** bincodec package initialization **************")
  private val touch: Unit = defaultSetup

  /** This sets the handler filter level,  all settings to modifiers are essentially overridden on level,
    * althought the modifiers may filter out additional things.
    * This is a no op if we think we are in continuous integration build
    * */
  def setTestLogLevel(l: Level): Unit = {
    if (!inCI) {
      scribe.warn(s"Setting all to log level $l")
      scribe.Logger.root.clearHandlers().withHandler(minimumLevel = Some(l)).replace()
      //scribe.Logger.root.clearModifiers().withMinimumLevel(l).replace()
    }
  }

  protected def inCI: Boolean = scala.sys.env.getOrElse("CONTINUOUS_INTEGRATION", "false") === "true"

  /** Scala test should manuall control after this */
  lazy val defaultSetup: Unit = {

    if (inCI) { // This should catch case when as a library in someone elses CI
      scribe.info("defaultSetup for logging IN CONTINUOUS_INTEGRATION")
      setTestLogLevel(Warn)
    } else {
      setTestLogLevel(Debug) // On Assumption we are in library mode, not testing, which will override.
    }
    scribe.info("Done with Default")
  }

  // Well, as far as I can tell the flow is: Logger => Modifiers => Handlers, The handlers write with Formatters
  // but the default console handler (at least) can also filter with minimumLogLevel
  // This experiment has scribe.Logger.root set at DEBUG.
  // We want to filter the debug messages just for com.odenzo.ripple.bincodec.reference.FieldInfo
  // method encodeFieldID but do just for package s
  def replaceModifiers(packages: List[String], l: Level): Unit = {
    scribe.info(s"Setting Packages Level to $l")
    val pri = Priority.Normal // unnecessary since clearing existing modifiers, but handy for future.
    scribe.Logger.root.clearModifiers().withModifier(LoggingConfig.excludePackageSelction(packages, l, pri)).replace()

  }

}
