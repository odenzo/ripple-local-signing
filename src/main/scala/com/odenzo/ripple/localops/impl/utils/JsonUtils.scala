package com.odenzo.ripple.localops.impl.utils

import io.circe.optics.JsonPath
import io.circe.syntax._
import io.circe.{Json, JsonObject, Printer}

import cats.Monoid
import cats.implicits._

import com.odenzo.ripple.localops.{ErrorHandling, LOpJsonErr, LocalOpsError}

trait JsonUtils extends CirceUtils {

  /** Monoid/Semigroup for Circe Json Object so we can add them togeher. */
  implicit val jsonObjectMonoid: Monoid[JsonObject] = new Monoid[JsonObject] {
    def empty: JsonObject = JsonObject.empty

    def combine(x: JsonObject, y: JsonObject): JsonObject = JsonObject.fromIterable(x.toVector |+| y.toVector)
  }

  def hasField(name: String, json: Json): Boolean = {
    findField(name, json).isRight
  }

  def findField(name: String, json: Json): Either[LOpJsonErr, Json] = {
    JsonPath.root.at(name).getOption(json).flatten.toRight(LocalOpsError(s"Field $name not found ", json.asJson))
  }

  def findFieldAsString(name: String, json: Json): Either[LOpJsonErr, String] =
    findField(name, json).flatMap(json2string)

  def findFieldAsObject(name: String, json: Json): Either[LOpJsonErr, JsonObject] =
    findField(name, json).flatMap(json2object)

  /** Ripled doesn't like objects like { x=null } */
  val droppingNullsPrinter: Printer = Printer.spaces2.copy(dropNullValues = true)

  def removeAllNulls(j: JsonObject): ErrorHandling.ErrorOr[JsonObject] = {
    val cleanTxt = droppingNullsPrinter.print(j.asJson)
    parseAsJson(cleanTxt).flatMap(json2object)
  }

}

object JsonUtils extends JsonUtils
