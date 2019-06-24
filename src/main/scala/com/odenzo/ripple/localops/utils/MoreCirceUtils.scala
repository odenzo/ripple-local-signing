package com.odenzo.ripple.models.utils

import io.circe.{ACursor, Json, JsonObject}


import com.odenzo.ripple.localops.utils.caterrors.{AppError, OError}
import com.odenzo.ripple.localops.utils.caterrors.CatsTransformers.ErrorOr

import cats._
import cats.data._
import cats.implicits._

trait CirceEncoderUtils {


  /**
    * Generic lifter being applied just to ledger default encoding for now. The default encoder will
    * make a Ledger subobject in Json, with differing fields based on the concrete instance Ledger subtype.
    * (i.e LedgerIndex, LedgerHash, LedgerId, LedgerName...)
    *
    * @param parent
    * @param subfield Expected that this feildname exists and is a JSONObject.
    *                 If not then no changes are made to incoming parent.
    *
    * @return
    */
  def liftLedgerFields(parent: JsonObject, subfield: String): JsonObject = {
    import com.odenzo.ripple.localops.utils.CirceUtils._
    val updatedObj: Option[JsonObject] = parent(subfield).flatMap { ov⇒ov.asObject
       match {
       case None ⇒ None
       case Some(obj) ⇒ Some( obj |+| parent.remove(subfield))
     }
    }
     updatedObj.getOrElse(parent)
  }

}

trait CirceDecoderUtils

object CirceDecoderUtils extends CirceDecoderUtils

trait CirceCodecUtils extends CirceEncoderUtils with CirceDecoderUtils {

  type KeyTransformer = String => String

  /** This is the transforms from io.circe.generic.extras */
  val snakeCaseTransformation: KeyTransformer =
    _.replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2").replaceAll("([a-z\\d])([A-Z])", "$1_$2").toLowerCase

  /** Capitalize a somewhat normal word */
  val capitalize: KeyTransformer = _.capitalize

  val unCapitalize: KeyTransformer = { toString: String =>
    if (toString == null) null
    else if (toString.length == 0) toString
    else if (toString.charAt(0).isLower) toString
    else {
      val chars = toString.toCharArray
      chars(0) = chars(0).toLower
      new String(chars)
    }
  }
  /** Typically used (slowly) for encoders. Possibly better to use derived or generic-extras
    * Note that it has a white-list of fields not to upcase (e.g. hash)
    **/
  def upcaseFields(obj: JsonObject): JsonObject = {
    // Could optimize this a bit by checking if key.head.isUpper or something I bet.
    // And also case (key,json) if !whitelist.contains(key)
    // Also design these generic and maybe so we can compose on fields
    val upcasedFirst = obj.toList.map {
      case (key, json)  => (key.head.toUpper + key.tail, json)
      case other                                  => other
    }
    JsonObject.fromIterable(upcasedFirst)
  }

  /** Idea here is to partially apply this with any (oldName,newName) entries you want. If name not found then no
    * changes to name.
    * For instance, to capitalize all and change some_oddball to SomeOddball
    * {{{
    *   val myMap = Map[String,String] ( "SomeOddball" -> "some_oddball")
    *   val customerOnly = customerNameTransformer(myMap)
    *   val composeStuff = capitalize.compose(customerOnly) // or x compose y style
    *   val moreReadbleFn = customerOnly.andThen(capitalize)
    * }}}
    *
    * @param map
    * @param name
    *
    * @return
    */
  def customNameTransformer(map: Map[String, String], name: String): String = map.getOrElse(name, name)

  def upcaseExcept(these: List[String]): (String) => String = { (key: String) =>
    val newKey = if (!these.contains(key)) key.capitalize else key
    //logger.debug(s"Converted $key -> $newKey")
    newKey
  }

  /** Caution that this must be done AFTER any general transaction of names for top level */
  def liftJsonObject(obj: JsonObject, field: String): JsonObject = {
    // Relies RippleTransaction being encoded as JsonObject
    obj(field).flatMap(_.asObject) match {
      case None            => obj
      case Some(objToLift) => JsonObject.fromIterable(obj.remove(field).toList ::: objToLift.toList)
    }
  }

  /** Usage {{{
  *
  * }}}
  */

  def fieldNameChangeEx(name: String, newName: String)(in: JsonObject): JsonObject = {
    // If missing existing name return JsonObject unchanges.
    // If oldname == null then i guess will add newName : null
    val updated: Option[JsonObject] = in(name)
      .map(oldVal ⇒ in.add(newName, oldVal))
      .map(jo ⇒ jo.remove(name))
    updated.getOrElse(in)
  }

  def changeObjectField(oldName:String,newName:String): ACursor ⇒ ACursor = {
    prepareJsonObject(fieldNameChangeEx(oldName,newName))
  }

  /** *
    * {{{
    *     val changer = fieldNameChangeEx("oldName","newName")
    *     Decoder[A].prepare(prepareJsonObject(changer))
    * }}}
    *
    * @param fn
    * @param in
    *
    * @return
    */
  def prepareJsonObject(fn: JsonObject => JsonObject)(in: ACursor): ACursor = {
    in.withFocus(json ⇒ json.mapObject(jobj ⇒ fn(jobj)))
  }

  def extractFieldFromObject(jobj: JsonObject, fieldName: String): Either[OError, Json] = {
    Either.fromOption(jobj.apply(fieldName), AppError(s"Could not Find $fieldName in JSonObject "))
  }

  /**
    * Little utility for common case where an JsonObject just has "key": value
    * WHere value may be heterogenous?
    *
    * @param json
    */
  def extractAsKeyValueList(json: Json): ErrorOr[List[(String, Json)]] = {
    val obj: Either[OError, JsonObject]           = json.asObject.toRight(AppError("JSON Fragment was not a JSON Object"))
    val ans: Either[OError, List[(String, Json)]] = obj.map(_.toList)
    ans
  }

  /**
    * Parses the list of json key  value pairs until it hits first error (not-accumulating parsing).
    *
    * @param json
    * @param fn
    * @tparam T
    *
    * @return
    */
  def parseKeyValuesList[T](json: Json, fn: (String, Json) ⇒ Either[AppError, T]): ErrorOr[List[T]] = {
    val kvs: ErrorOr[List[(String, Json)]] = extractAsKeyValueList(json)
    // kvs.flatTraverse( ??)
    kvs.flatMap { theList ⇒
      theList.traverse { case (key:String, value:Json) ⇒ fn(key, value)}
    }
  }
}

object CirceCodecUtils extends CirceCodecUtils
