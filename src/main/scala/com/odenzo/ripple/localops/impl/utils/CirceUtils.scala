package com.odenzo.ripple.localops.impl.utils

import java.io.File

import cats._
import cats.implicits._
import io.circe._
import io.circe.jawn.JawnParser
import io.circe.syntax._
import scribe.Logging

import com.odenzo.ripple.localops.impl.utils.caterrors.ErrorHandling.ErrorOr
import com.odenzo.ripple.localops.impl.utils.caterrors.{
  AppError,
  AppException,
  AppJsonDecodingError,
  AppJsonParsingError
}

/**
  * Traits for working with Circe DOM
  */
trait CirceUtils extends Logging {

  /** Ripled doesn't like objects like { x=null }
    *    {{{
    *      droppingNullsSortedPrinter.pretty(json)
    *    }}}
    */
  val droppingNullsSortedPrinter: Printer = Printer.spaces2SortKeys.copy(dropNullValues = true)

  /** Recurses through the JsonObjects and drops all null fields.
    * Equivalent to printing with droppingNullsPrinter and parsing again.
    * Note: Doesn't recurse down arrays
    * This is used for testing and things, better to use dropingNullsPrinter I think.
    */
  def pruneNullFields(obj: JsonObject): JsonObject = {
    // Explicit, but there is no way this should be an error.

    json2jsonObject(obj.asJson.dropNullValues) match {
      case Left(e)  => obj
      case Right(v) => v
    }

  }

  /** This probably doesn't preserve the ordering of fields in Object.  */
  def replaceField(name: String, in: JsonObject, withValue: Json): JsonObject = {
    in.remove(name).add(name, withValue)
  }

  /** Does top level sorting of fields in this object alphanumeric with capital before lowercase  */
  def sortFields(obj: JsonObject): JsonObject = {

    json2jsonObject(obj.asJson.dropNullValues) match {
      case Left(e)  => obj
      case Right(v) => v
    }
  }

  /** Sorts top level object and all nested fields Doesn't traverse down arrays though (needed!)*/
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
  def parseAsJson(m: String): Either[AppJsonParsingError, Json] = {

    io.circe.parser.parse(m).leftMap { pf =>
      new AppJsonParsingError("Error Parsing String to Json", m, pf)
    }
  }

  def parseAndDecode[T](m: String, decoder: Decoder[T]): Either[Throwable, T] = {
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

  def decode[T](json: Json, decoder: Decoder[T]): Either[AppJsonDecodingError, T] = {
    decoder.decodeJson(json).leftMap((e: DecodingFailure) => AppError(s"Using Decoder $decoder for Type", json, e))
  }

  def decode[T](jsonObj: JsonObject, decoder: Decoder[T]): Either[AppError, T] = {
    val json = jsonObj.asJson
    decode(json, decoder)
  }
}

object CirceUtils extends CirceUtils
