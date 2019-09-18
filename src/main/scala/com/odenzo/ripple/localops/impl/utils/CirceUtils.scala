package com.odenzo.ripple.localops.impl.utils

import java.io.File

import io.circe._
import io.circe.jawn.JawnParser
import io.circe.syntax._

import cats._
import cats.implicits._
import monocle.Optional
import scribe.Logging

import com.odenzo.ripple.localops.ErrorHandling.ErrorOr
import com.odenzo.ripple.localops.{LOpException, LOpJsonErr, LocalOpsError}

/**
  * Traits for working with Circe DOM
  */
trait CirceUtils extends Logging {

  /** Ripled doesn't like objects like { x=null } and neither does Binary-Codec lib
    *    {{{
    *      droppingNullsSortedPrinter.pretty(json)
    *    }}}
    */
  val droppingNullsSortedPrinter: Printer = Printer.spaces2SortKeys.copy(dropNullValues = true)

  /** Intended to check if an JsonObject. Used in flatTap alot  */
  def ensureJsonObject(a: Json) = {
    val shifted: Either[LOpJsonErr, JsonObject] = a.asObject.toRight(LocalOpsError("Encuring JObj aFailed", a))
    shifted.map(v => true)
  }

  /**
    * Prunes null fields by using null printer since I don't want to recurse arrays myself.
    * This is slow, but the only reason we use is for testing anyway
    */
  def pruneNullFields(obj: Json): Either[LOpException, Json] = {
    // Explicit, but there is no way this should be an error.
    val printer: Printer = Printer.spaces4.copy(dropNullValues = true)
    val pretty: String   = printer.print(obj.asJson)
    parseAsJson(pretty)

  }

  /** This probably doesn't preserve the ordering of fields in Object.  */
  def replaceField(name: String, in: JsonObject, withValue: Json): JsonObject = {
    in.remove(name).add(name, withValue)
  }

  /** Does top level sorting of fields in this object alphanumeric with capital before lowercase
    * See if circe sorted fields does this nicely */
  def sortFields(obj: JsonObject): JsonObject = {
    JsonObject.fromIterable(obj.toVector.sortBy(_._1))
  }

  /** This does not recurse down, top level fields only */
  def sortFieldsDroppingNulls(obj: JsonObject): JsonObject = {
    val iter = obj.toVector.filter(_._2 =!= Json.Null).sortBy(_._1)
    JsonObject.fromIterable(iter)
  }

  def dropNullValues(obj: JsonObject): JsonObject = {
    val iter = obj.toVector.filter(_._2 =!= Json.Null)
    JsonObject.fromIterable(iter)
  }

  /** Caution: Uses BigDecimal and BigInt in parsing.
    *
    * @param m The text, in this case the response message text from websocket.
    *
    * @return JSON or an exception if problems parsing, error holds the original String.
    */
  def parseAsJson(m: String): Either[LOpException, Json] = {
    io.circe.parser.parse(m).leftMap(pf => LOpException(pf.message, pf.underlying))
  }

  def parseAndDecode[T](m: String, decoder: Decoder[T]): Either[LocalOpsError, T] = {
    parseAsJson(m).flatMap(decode(_, decoder))
  }

  def parseAsJson(f: File): ErrorOr[Json] = {
    logger.info(s"Parsing File $f")
    new JawnParser().parseFile(f).leftMap(pf => new LOpException(s"Parsing File $f", pf))
  }

  def json2object(json: Json): Either[LOpJsonErr, JsonObject] = {
    json.asObject.toRight(LocalOpsError("JSON was not a JSonObject", json))
  }

  def json2array(json: Json): Either[LOpJsonErr, Vector[Json]] = {
    json.asArray.toRight(LocalOpsError("JSON was not a Array", json))
  }

  def json2string(json: Json): Either[LOpJsonErr, String] = {
    json.asString.toRight(LocalOpsError("JSON was not a String", json))
  }

  def decode[T](json: Json, decoder: Decoder[T]): Either[LOpJsonErr, T] = {
    decoder.decodeJson(json).leftMap(e => LocalOpsError(s"Decoder $decoder failed", json, e))
  }

  def decode[T](jsonObj: JsonObject, decoder: Decoder[T]): Either[LOpJsonErr, T] = {
    decode(jsonObj.asJson, decoder)
  }

  def lensGetOpt[T](lens: Optional[Json, T])(applyTo: Json): Either[LOpJsonErr, T] = {
    lens.getOption(applyTo).toRight(LocalOpsError(s"Path Failed ${lens.toString}", applyTo))
  }
}

object CirceUtils extends CirceUtils
