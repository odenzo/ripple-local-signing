package com.odenzo.ripple.localops.testkit

import cats.Eval
import cats.implicits._
import scribe.Level.Warn
import scribe.{Level, Logging, Priority}

import com.odenzo.ripple.localops.impl.utils.ScribeLogUtils
import com.odenzo.ripple.localops.testkit.OTestLogging.setTestLogLevel

trait OTestLogging extends Logging {

  // When exactly does this get instanciated? Have to touch it.

  // Need to apply these by package scope for library mode.
  // APply to com.odenzo.ripple.bincodec.*

  scribe.debug("*********** localops  OTestLogging  initialization **************")
  DefaultSettings.defaultSetup.value

  /** This sets the handler filter level,  all settings to modifiers are essentially overridden on level,
    * althought the modifiers may filter out additional things.
    * This is a no op if we think we are in continuous integration build
    * */
  def setTestLogLevel(l: Level): Unit = {
    if (!OTestLogging.inCI) {
      scribe.warn(s"Setting all to log level $l")
      scribe.Logger.root.clearHandlers().withHandler(minimumLevel = Some(l)).replace()
    }
  }
}

object DefaultSettings extends ScribeLogUtils {

  scribe.warn("DefaultSettings Object Initialized")

  /** Scala test should manuall control after this. Executed only once, lazy and memoized */
  val defaultSetup: Eval[Level] = Eval.later {

    val threshold = if (inCI) { // This should catch case when as a library in someone elses CI
      scribe.warn("defaultSetup for logging IN CONTINUOUS_INTEGRATION")
      Warn
    } else {
      Warn // On Assumption we are in library mode, not testing, which will override.
    }
    scribe.warn(s"defaultSetup for test logging $threshold")
    setTestLogLevel(threshold)

    val makeQuiet = List(
      "com.odenzo.ripple.bincodec.reference",
      "com.odenzo.ripple.bincodec.utils"
    )

    applyFilter(excludePackageSelction(makeQuiet, Level.Warn, Priority.Highest))

    applyFilter(excludeByClass(classOf[OTestLogging], Level.Debug))
    threshold
  }
  private val inCI: Boolean = scala.sys.env.getOrElse("CONTINUOUS_INTEGRATION", "false") === "true"
}

object OTestLogging extends OTestLogging {

  logger.warn("OTestLogging Object Initializing")
  protected def inCI: Boolean = scala.sys.env.getOrElse("CONTINUOUS_INTEGRATION", "false") === "true"

}
