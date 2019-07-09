package com.odenzo.ripple

import scribe.Level
import scribe.Level.Warn

import com.odenzo.ripple.bincodec.inCI



package object localops {


  scribe.warn("*********** bincodec package initialization **************")
  private val touch: Unit = defaultSetup


  /** Scala test should manuall control after this */
  lazy val defaultSetup: Unit = {

    if (inCI) { // This should catch case when as a library in someone elses CI
      setAllToLevel(Warn)
    } else {
      setAllToLevel(Warn) // On Assumption we are in library mode, not testing, which will override.
    }
    scribe.error("Done with Default")
  }

  /** This sets the handler filter level,  all settings to modifiers are essentially overridden on level,
    * althought the modifiers may filter out additional things.
    *
    * */
  def setAllToLevel(l: Level): Unit = {
    scribe.warn(s"Setting all to log level $l")
    scribe.Logger.root.clearHandlers().withHandler(minimumLevel = Some(l)).replace()
    //scribe.Logger.root.clearModifiers().withMinimumLevel(l).replace()
  }

}
