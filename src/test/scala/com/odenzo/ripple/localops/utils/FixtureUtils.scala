package com.odenzo.ripple.localops.utils

import cats._
import cats.data._
import cats.implicits._
import io.circe.{Decoder, Json, JsonObject}

import com.odenzo.ripple.localops.OTestSpec

trait FixtureUtils extends JsonUtils with OTestSpec {

  /** All fields in request are strings */
  def req2field(fix: JsonObject, fieldName: String): Option[String] = {
    fix("Request").flatMap(_.asObject).flatMap(v ⇒ v(fieldName)).flatMap(_.asString)
  }

  /** All fields in respone are strings */
  def res2field(fix: JsonObject, fieldName: String): Option[String] = {
    val result: Option[JsonObject] = fix("Response").flatMap(_.asObject).flatMap(v ⇒ v("result")).flatMap(_.asObject)
    val field                      = result.flatMap(v ⇒ v(fieldName))
    field.flatMap(_.asString)
  }

  def findRequiredStringField(name: String, jobj: JsonObject): String = {
    getOrLog(findField(name, jobj).flatMap(json2string))
  }

  /**
    *
    * @return Json of field or logging of error and assertion failure
    */
  def findRequiredField(name: String, json: JsonObject): Json = {
    getOrLog(findField(name, json))
  }


  def findRequiredObject(name: String, jsonObject: JsonObject): JsonObject = {
    val asObj = findField(name, jsonObject).flatMap(json2object)
    getOrLog(asObj)
  }



  def streamFullAccountKeys() = {
    import cats.effect.IO
    import io.circe.iteratee.{byteArrayParser, decoder}
    import io.iteratee.{Enumeratee, Iteratee}
    import io.iteratee.files.readBytes
    import java.io.File

   // val lots = readBytes(new File("data.json")).through(byteArrayParser).through(Decoder[JsonObject])
  }
  /**
    *
    * @param resource
    *
    * @return List of Json Requests and Responses tupled.
    */
  def loadRequestResponses(resource: String): List[(JsonObject, JsonObject)] = {

    val txnfixture: Json          = getOrLog(loadJsonResource(resource), s"Loading RR from $resource")
    val fixObjs: List[JsonObject] = getOrLog(CirceUtils.decode(txnfixture, Decoder[List[JsonObject]]))

    fixObjs.map { obj ⇒
      val req = findRequiredObject("request", obj)
      val res = findRequiredObject("response", obj)
      (req, res)
    }

  }


}
