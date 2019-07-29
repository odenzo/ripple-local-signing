package com.odenzo.ripple.localops.impl.utils

import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}

import com.odenzo.ripple.localops.impl.Signer.{json2array, json2object}
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError

trait JsonUtils extends CirceUtils {

  def findField(name: String, json: JsonObject): Either[AppError, Json] = {
    Either.fromOption(json(name), AppError(s"Field $name not found ", json.asJson))
  }

  def findObjectField(name: String, json: JsonObject): Either[AppError, JsonObject] = {
    findField(name, json).flatMap(json2object)
  }

  def findStringField(name: String, jobj: JsonObject): Either[AppError, String] = {
    findField(name, jobj).flatMap(json2string)
  }

  /**
    * Tries to find the given field name in the object and get as a String.
    *
    * @param fieldName
    * @param obj
    *
    * @return None if no field present or it is not a String, else Some(fieldname,val)
    */
  def getFieldAsString(fieldName: String, obj: JsonObject): Option[(String, String)] = {
    obj(fieldName).flatMap(_.asString).tupleLeft(fieldName)
  }

  def json2object(json: Json): Either[AppError, JsonObject] = {
    Either.fromOption(json.asObject, AppError("Expected JSON Object", json))
  }

  def json2array(json: Json): Either[AppError, List[Json]] = {
    Either.fromOption(json.asArray.map(_.toList), AppError("Expected JSON Array", json))
  }

  def json2arrayOfObjects(json: Json): Either[AppError, List[JsonObject]] = {
    json2array(json).flatMap(ls => ls.traverse(json2object))
  }

  def json2string(json: Json): Either[AppError, String] = {
    Either.fromOption(json.asString, AppError("Expected JSON String", json))
  }

}

object JsonUtils extends JsonUtils
