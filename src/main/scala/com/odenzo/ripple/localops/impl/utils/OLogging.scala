package com.odenzo.ripple.localops.impl.utils
import io.circe.syntax._
import io.circe.{Json, JsonObject}

import cats._
import cats.data._
import cats.implicits._
import scribe._

trait OLogging {

  val logger = scribe.Logger.logger

  /**
    *  Debug print a few cmmon types or delegate to pretty printer
    *  I want to handle Showable and non-Showable, how to check at runtime if showable?
    *  So, it will now use show :-)
    * @param a
    * @tparam T
    * @return
    */
  def dprint[T](a: T): String = {
    a match {
      case j: Json       => j.spaces4
      case j: JsonObject => j.asJson.spaces4
      case general       => pprint.apply(a).render
    }
  }

}
