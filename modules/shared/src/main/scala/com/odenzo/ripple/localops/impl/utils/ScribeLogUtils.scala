package com.odenzo.ripple.localops.impl.utils

import cats._
import cats.data._
import cats.implicits._
import scribe.filter._
import scribe.modify.{LevelFilter, LogModifier}
import scribe.{Level, Logger, Priority}

/**
  * Scribe has run-time configuration.
  * This is designed to control when developing the codec library and also when using.
  * This is my experiment and learning on how to control
  * The default config favor scribe is INFO
  * See com.odenzo.ripple.bincodec package information for usage.
  * Want to find a stack based way or chaning logger config on root and popping back to previous
  */
trait ScribeLogUtils {

  /** We want to be very quiet in CI builds and don't let debugging logging interfere */
  def inCITesting: Boolean = {
    val travisTag = scala.sys.env.getOrElse("CONTINUOUS_INTEGRATION", "false")
    val localTag  = scala.sys.env.getOrElse("CI", "false")
    (localTag === "true" || travisTag === "true")
  }

  def defaultLevel(): Level.Warn.type = {
    Level.Warn
  }

  val runtimeLogging: LevelFilter   = LevelFilter.>=(Level.Warn)
  val ciLogging: LevelFilter        = LevelFilter.>=(Level.Warn)
  val testLogging: LevelFilter      = LevelFilter.>=(Level.Warn)
  val testDebugLogging: LevelFilter = LevelFilter.>=(Level.Debug)

  /** This sets the handler filter level,  all settings to modifiers are essentially overridden on level,
    * althought the modifiers may filter out additional things.
    * This is a no op if we think we are in continuous integration build
    * */
  def setTestLogLevel(l: Level): Unit = {
    if (!inCITesting) setLogLevel(l)
    ()
  }

  def setLogLevel(l: Level): Logger = {
    scribe.warn(s"Setting all to log level $l")
    // replace is needed.
    scribe.Logger.root.clearHandlers().withHandler(minimumLevel = Some(l)).replace()
  }

  /** Helper to filter out messages in the packages given below the given level
    * I am not sure this works with the global scribe object or not.
    *
    * select(
    *packageName.startsWith("no.officenet"),
    *packageName.startsWith("com.visena")
    * )
    * Usage:
    * {{{
    *   scribe.
    * }}}
    *
    * @return a filter that can be used with .withModifier() */
  def excludePackageSelction(packages: List[String], atOrAboveLevel: Level, priority: Priority): FilterBuilder = {
    val ps: List[Filter] = packages.map(p => packageName.startsWith(p))
    val fb               = select(ps: _*).exclude(level < atOrAboveLevel).includeUnselected.copy(priority = priority)
    fb
  }

  def excludeByClasses(clazzes: List[Class[_]], minLevel: Level): FilterBuilder = {
    val names = clazzes.map(_.getName)
    scribe.info(s"Filtering Classes: $names to $minLevel")
    val filters = names.map(n => className(n))
    select(filters: _*).include(level >= minLevel)
  }

  /**
    * Creates an exclude filter for the given class name that anything under the minLevel is filtered.
    * This needs to be applied (generally to the root logger). Set as highest priority.
    *
    * @param clazz
    * @param minLevel
    *
    * @return
    */
  def excludeByClass(clazz: Class[_], minLevel: Level): FilterBuilder = {
    val name = clazz.getName
    scribe.warn(s"Filtering Class: $name to $minLevel")
    val filter = className(name)
    select(filter).exclude(level < minLevel).priority(Priority.Highest)
  }

  /** FilterBuilder is a LogModifier */
  def applyFilter(filter: FilterBuilder): Logger = {
    applyLogModifier(filter)
  }

  def applyLogModifier(mod: LogModifier): Logger = {
    scribe.Logger.root.withModifier(mod).replace() // Not sure why the replace is needed, or if it is needed!?
  }

  // Well, as far as I can tell the flow is: Logger => Modifiers => Handlers, The handlers write with Formatters
  // but the default console handler (at least) can also filter with minimumLogLevel
  // This experiment has scribe.Logger.root set at DEBUG.
  // We want to filter the debug messages just for com.odenzo.ripple.bincodec.reference.FieldInfo
  // method encodeFieldID but do just for package s
  def replaceModifiers(packages: List[String], l: Level): Unit = {
    scribe.info(s"Setting Packages Level to $l")
    val pri = Priority.Normal // unnecessary since clearing existing modifiers, but handy for future.
    val replaced = scribe.Logger.root
      .clearModifiers()
      .withModifier(ScribeLogUtils.excludePackageSelction(packages, l, pri))
      .replace()
    ()
  }
}

object ScribeLogUtils extends ScribeLogUtils
