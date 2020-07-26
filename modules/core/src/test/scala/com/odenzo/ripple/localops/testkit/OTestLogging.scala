package com.odenzo.ripple.localops.testkit

import cats._
import cats.data._
import cats.implicits._
import scribe.Level.{Debug, Warn}
import scribe.{Level, Logger, Logging, Priority}

import com.odenzo.ripple.localops.impl.utils.OLogging
import com.odenzo.ripple.localops.testkit.OTestSpec.logger

trait OTestLogging extends OLogging {

  // When exactly does this get instanciated? Have to touch it.

  // Need to apply these by package scope for library mode.
  // APply to com.odenzo.ripple.bincodec.*

  scribe.debug("*********** localops  OTestLogging  initialization **************")
  DefaultTestLogging.defaultSetup.value
  def setTestLogLevel(l: Level): Unit = if (!inCITesting) setLogLevel(l)
  def resetTestLogging(l: Level)      = if (!inCITesting) resetTo(l)

  /** We want to be very quiet in CI builds and don't let debugging logging interfere */
  def inCITesting: Boolean = {
    val travisTag = scala.sys.env.getOrElse("CONTINUOUS_INTEGRATION", "false")
    val localTag  = scala.sys.env.getOrElse("CI", "false")
    (localTag === "true" || travisTag === "true")
  }
}

object DefaultTestLogging extends OLogging {

  /** Scala test should manuall control after this. Executed only once, lazy and memoized */
  val defaultSetup: Eval[Level] = Eval.later {
    val threshold = if (OTestLogging.inCITesting) { // This should catch case when as a library in someone elses CI
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
    super.mutePackages(makeQuiet)
    threshold
  }

  if (!OTestLogging.inCITesting) {
    scribe.warn("Think I am in CI so all logging at Error Level")
    scribe.Logger.root.clearHandlers().clearModifiers().withHandler(minimumLevel = Some(Level.Error)).replace()
  }
}

object OTestLogging extends OTestLogging {

  scribe.warn("OTestLogging Object Initializing")

}
