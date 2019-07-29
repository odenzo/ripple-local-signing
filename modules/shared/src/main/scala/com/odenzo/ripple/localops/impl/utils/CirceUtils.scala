package com.odenzo.ripple.localops.impl.utils

import java.io.File

import cats._
import cats.implicits._
import io.circe._
import io.circe.jawn.JawnParser
import io.circe.syntax._
import scribe.Logging

import com.odenzo.ripple.localops.impl.utils.caterrors.CatsTransformers.ErrorOr
import com.odenzo.ripple.localops.impl.utils.caterrors.{
  AppError,
  AppException,
  AppJsonDecodingError,
  AppJsonParsingError
}

/**
  * Traits for working with Circe DOM [[io.circe.Json]]
  */
trait CirceUtils extends Logging {

  /** Ripled doesn't like objects like { x=null } */
  val droppingNullsPrinter: Printer = Printer.spaces2.copy(dropNullValues = true)

  /** Monoid/Semigroup for Circe Json Object so we can add them togeher. */
  implicit val jsonObjectMonoid: Monoid[JsonObject] = new Monoid[JsonObject] {
    def empty: JsonObject = JsonObject.empty

    def combine(x: JsonObject, y: JsonObject): JsonObject = JsonObject.fromIterable(x.toVector |+| y.toVector)
  }

  /** Converts json to formatted text dropping null JsonObject fields.
    *
    * @param json
    *
    * @return
    */
  def print(json: Json): String = json.pretty(droppingNullsPrinter)

  def printObj(jsonObject: JsonObject): String = print(jsonObject.asJson)

  /** For now does top level pruning of null fields from JSON Object
    * Now recurses */
  def pruneNullFields(obj: JsonObject): JsonObject = {
    obj
      .filter {
        case (_, Json.Null) => false
        case other          => true
      }
      .mapValues { js: Json =>
        js.asObject match {
          case Some(v) => pruneNullFields(v).asJson
          case None    => js
        }
      }
      .asJsonObject

  }

  def replaceField(name: String, in: JsonObject, withValue: Json): JsonObject = {
    in.remove(name).add(name, withValue)
  }

  /** Does top level sorting of fields in this object alphanumeric with capital before lowercase  */
  def sortFields(jsonObject: JsonObject): JsonObject = {
    // Want Capital letters sorted before lower case
    val sortedList = jsonObject.toList.sortBy { case (fieldName: String, _: Json) => fieldName }
    JsonObject.fromIterable(sortedList)
  }

  /** Sorts top level object and all nested fields */
  def sortDeepFields(jsonObject: JsonObject): JsonObject = {
    val deep = jsonObject.mapValues { iv: Json =>
      iv.asObject match {
        case None      => iv
        case Some(obj) => sortDeepFields(obj).asJson
      }
    }
    sortFields(deep)
  }

  /** Caution: Uses BigDecimal and BigInt in parsing.
    *
    * @param m The text, in this case the response message text from websocket.
    *
    * @return JSON or an exception if problems parsing, error holds the original String.
    */
  def parseAsJson(m: String): ErrorOr[Json] = {

    io.circe.parser.parse(m).leftMap { pf =>
      new AppJsonParsingError("Error Parsing String to Json", m, pf)
    }
  }

  def parseAndDecode[T](m: String, decoder: Decoder[T]): Either[AppError, T] = {
    parseAsJson(m).flatMap(decode(_, decoder))
  }

  def parseAsJson(f: File): ErrorOr[Json] = {
    logger.info(s"Parsing File $f")
    new JawnParser().parseFile(f).leftMap { pf =>
      new AppException(s"Error Parsing File $f to Json", pf)
    }
  }

  def parseAsJsonObject(m: String): ErrorOr[JsonObject] = {
    parseAsJson(m).flatMap(json2jsonObject)
  }

  def json2jsonObject(json: Json): ErrorOr[JsonObject] = {
    Either.fromOption(json.asObject, AppError("JSON was not a JSonObject"))
  }

  def decode[T](json: Json, decoder: Decoder[T]): ErrorOr[T] = {
    //val targs = typeOf[T] match { case TypeRef(_, _, args) => args }
    //val tmsg = s"type of $decoder has type arguments $targs"

    val decoderInfo = decoder.toString
    val msg         = s"Using Decoder $decoderInfo for Type"
    decoder.decodeJson(json).leftMap((e: DecodingFailure) => new AppJsonDecodingError(json, e, msg))
  }

  def decode[T](jsonObj: JsonObject, decoder: Decoder[T]): ErrorOr[T] = {
    val json = jsonObj.asJson
    decode(json, decoder)
  }
}

object CirceUtils extends CirceUtils
