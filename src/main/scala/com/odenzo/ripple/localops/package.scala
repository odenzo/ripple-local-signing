package com.odenzo.ripple

import cats._
import cats.data._
import cats.implicits._
import scribe.Level.Warn
import scribe.{Level, Logging}

package object localops extends Logging {

  scribe.warn("*********** localops package initialization **************")
  /** Scala test should manuall control after this */
  lazy val defaultSetup: Unit = {

    if (inCI) { // This should catch case when as a library in someone elses CI
      setAllToLevel(Warn)
    } else {
      setAllToLevel(Warn) // On Assumption we are in library mode, not testing, which will override.
    }
    logger.warn("Done with Default in localops ")
  }
  protected val inCI: Boolean = scala.sys.env.getOrElse("CONTINUOUS_INTEGRATION", "false") === "true"
  private val touch: Unit = defaultSetup

  /** This sets the handler filter level,  all settings to modifiers are essentially overridden on level,
    * althought the modifiers may filter out additional things.
    *
    * */
  def setAllToLevel(l: Level): Unit = {
    logger.warn(s"Setting all to log level $l")
    scribe.Logger.root.clearHandlers().withHandler(minimumLevel = Some(l)).replace()
  }

}
