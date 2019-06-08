package com.odenzo.ripple.localops.impl.utils.caterrors

import cats._
import cats.data._
import cats.implicits._
import io.circe.Decoder.Result
import io.circe._
import scribe.Level

import com.odenzo.ripple.localops.testkit.{FKP, OTestSpec}

class AppErrorTest extends OTestSpec {

  import AppErrorUtils.showThrowable

  customLogLevel = Level.Debug

  test("AppException") {

    val e     = AppError("Bad Data", new IllegalArgumentException("X was not present"))
    val error = e.show
  }

  test("Show Throwable") {
    val t: java.lang.Throwable = new IllegalArgumentException("hello")
    val msg                    = AppErrorUtils.showThrowable.show(t)
    val msg2                   = t.show
    msg shouldEqual msg2
  }

  test("JSONDecoding Error") {
    val json                       = """{ "foo": "bar" }"""
    val parsed: Json               = getOrLog(parseAsJson(json))
    val res: Either[AppError, FKP] = decode(parsed, Decoder[FKP])
    logger.debug(s"Result: $res")
    res.leftMap { v =>
      logger.debug(s"Error: ${v.show}")
      AppError("Test Json Bad", parsed, AppError("testMe"))
    }

    val manual: Result[FKP] = Decoder[FKP].decodeJson(parsed)
    AppError.wrapDecodingResult(manual, parsed, "Intentional")
  }

  test("JSONParsing Error") {
    val json                                   = """{ foo": "bar" }"""
    val res: Either[AppJsonParsingError, Json] = parseAsJson(json)
    logger.debug(s"Result: $res")
    res.leftMap(v => logger.debug(s"Error: ${v.show}"))

  }

}
