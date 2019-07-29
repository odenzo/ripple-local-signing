package com.odenzo.ripple.localops.testkit

import cats._
import cats.data._
import cats.implicits._
import io.circe.{Decoder, Json, JsonObject}

import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.impl.utils.{CirceUtils, JsonUtils}

trait FixtureUtils extends JsonUtils with OTestSpec {

  /** All fields in request are strings */
  def req2field(fix: JsonObject, fieldName: String): Option[String] = {
    fix("Request").flatMap(_.asObject).flatMap(v => v(fieldName)).flatMap(_.asString)
  }

  /** All fields in respone are strings */
  def res2field(fix: JsonObject, fieldName: String): Option[String] = {
    val result: Option[JsonObject] = fix("Response").flatMap(_.asObject).flatMap(v => v("result")).flatMap(_.asObject)
    val field                      = result.flatMap(v => v(fieldName))
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
    val asObj = findObjectField(name, jsonObject)
    getOrLog(asObj)
  }

  /**
    * Caution, this uses getOrLog which is helpful sometimes and evil othertimes.
    * Best used only when calling with ScalaTest test construct.
    *
    * @param resource
    *
    * @return List of Json Requests and Responses tupled.
    */
  def loadRequestResponses(resource: String): List[(JsonObject, JsonObject)] = {

    val txnfixture: Json          = getOrLog(loadJsonResource(resource), s"Loading RR from $resource")
    val fixObjs: List[JsonObject] = getOrLog(CirceUtils.decode(txnfixture, Decoder[List[JsonObject]]))

    fixObjs.map { obj =>
      val req = findRequiredObject("request", obj)
      val res = findRequiredObject("response", obj)
      (req, res)
    }

  }

  /**
    * Expects resource containing JSON array of objects, each object having a "request" and "response" field.
    *
    * @param resource
    *
    * @return List of JSONObject tuple for each request and response
    */
  def loadRqRsResource(resource: String): Either[AppError, List[(JsonObject, JsonObject)]] = {
    for {
      json <- loadJsonResource(resource)
      objs <- CirceUtils.decode(json, Decoder[List[JsonObject]])
      rr <- objs.traverse { o =>
        (findObjectField("request", o), findObjectField("response", o)).mapN((_, _))
      }
    } yield rr
  }

  /** Loads file of standard request response format for wallet propose and
    * decodes the result into AccountKeys. Keeps rq in raw format for no good reason.
    *
    * @param resource
    *
    * @return
    */
  def loadWalletRqRs(resource: String): Either[AppError, List[(JsonObject, AccountKeys)]] = {

    for {
      rr     <- loadRqRsResource(resource)
      rqKeys <- rr.traverse { case (rq, rs) => CirceUtils.decode(rs, AccountKeys.decoder).tupleLeft(rq) }
    } yield rqKeys
  }

}
