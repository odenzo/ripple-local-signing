package com.odenzo.ripple.localops.testkit

import cats.Eval
import scribe.Level.{Debug, Info, Warn}
import scribe.{Level, Logging, Priority}

import com.odenzo.ripple.localops.impl.utils.ScribeLogUtils

trait OTestLogging extends ScribeLogUtils with Logging {

  // When exactly does this get instanciated? Have to touch it.

  // Need to apply these by package scope for library mode.
  // APply to com.odenzo.ripple.bincodec.*

  scribe.debug("*********** localops  OTestLogging  initialization **************")
  DefaultTestLogging.defaultSetup.value

}

object DefaultTestLogging extends ScribeLogUtils {

  scribe.warn("DefaultTestLogging Object Initialized")

  /** Scala test should manuall control after this. Executed only once, lazy and memoized */
  val defaultSetup: Eval[Level] = Eval.later {
    scribe.warn(s"DefaultTestLogging Setup in Progress... In CI: ${inCITesting}")
    val threshold = if (inCITesting) { // This should catch case when as a library in someone elses CI
      scribe.warn("defaultSetup for logging IN CONTINUOUS_INTEGRATION")
      Warn
    } else {
      Debug
    }
    setLogLevel(threshold) // Sets even if in Continuous Integration mode

    val makeQuiet = List(
      "com.odenzo.ripple.bincodec.decoding.TxBlobBuster",
      "com.odenzo.ripple.bincodec.encoding.TypeSerializers",
      "com.odenzo.ripple.bincodec.reference",
      "com.odenzo.ripple.bincodec.utils"
    )
    scribe.warn(s"Muting Packages:\n" + makeQuiet.mkString("\n"))
    applyFilter(excludePackageSelction(makeQuiet, Level.Warn, Priority.Highest))

    applyFilter(excludeByClass(classOf[OTestLogging], Level.Warn))
    threshold
  }

}

object OTestLogging extends OTestLogging {

  logger.warn("OTestLogging Object Initializing")

}
