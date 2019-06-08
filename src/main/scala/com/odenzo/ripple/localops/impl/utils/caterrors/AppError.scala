package com.odenzo.ripple.localops.impl.utils.caterrors

import scala.util.{Failure, Success, Try}

import cats._
import cats.implicits._
import io.circe.Decoder.Result
import io.circe.{DecodingFailure, Json, ParsingFailure}
import scribe.Logging

import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError

/**
  * Base class that all errors (including OError) must extends directly or indirectly.
  * Not quite ready to move to case classes.
  */
sealed trait AppError extends Throwable { // Note this is not sealed and some shit defined elsewhere in the pasta bowl

  def msg: String
  def cause: Option[Throwable]

}

class OError(val msg: String = "No Message", val cause: Option[Throwable] = None) extends AppError

/** This should be terminal node only */
class AppException(val msg: String = "Wrapping Root Exception", val err: Throwable) extends AppError {
  val cause: Option[AppError] = Option.empty
}

/**
  * For use in Request/Response scenarios, or more generally when  a:Json => b:Json => transform(b) => answer
  * This can be used.
  * Prefer to use ORestCall when have source and response json at one place.
  */
class AppJsonError(val msg: String, val json: Json, val cause: Option[Throwable] = None) extends AppError {}

class AppJsonParsingError(val msg: String, val raw: String, val parser: ParsingFailure) extends AppError {
  val cause: Option[AppError] = new AppException(parser.message, parser).some
}

/**
  * Represents a error in Circe Json decoding (Json => Model)
  *
  * @param json JSON that was input, generally the complete document in scope
  * @param cause  The decoding failure from Circe.
  * @param note Informational message to enhance the exception, provides context.
  */
class AppJsonDecodingError(val json: Json, val cause: Option[DecodingFailure], val note: String = "") extends AppError {
  val msg: String  = note + ":" + cause.map(_.message)
  val base: String = s"\n OR: ${cause.show}"
}

/**
  * Base Error is never instanciated, but the apply is up here as convenience
  * and delegates down. These will go away soon.
  */
object AppError extends Logging {

  // Builders for consistency and hiding a bit of the ever chaning mess.

  def apply(msg: String, err: Throwable): AppException = new AppException(msg, err)

  def apply(msg: String, json: Json): AppJsonError = new AppJsonError(msg, json)

  def apply(msg: String, json: Json, cause: AppError): AppJsonError = new AppJsonError(msg, json, Some(cause))

  def apply(m: String): OError = new OError(m, None)

  def apply(msg: String, json: Json, err: DecodingFailure): AppJsonDecodingError =
    new AppJsonDecodingError(json, Some(err), msg)

  /**
    * Wrap the Circe Decoding error if there was one, and return as Either
    *
    * @param v
    * @param json
    * @param note
    * @tparam T
    *
    * @return
    */
  def wrapDecodingResult[T](v: Result[T], json: Json, note: String = "No Clues"): Either[AppJsonDecodingError, T] = {
    v.leftMap { err: DecodingFailure =>
      new AppJsonDecodingError(json, Some(err), note)
    }
  }

  /**
    *  RippleBindaryCodec error is from library and extends Throwable but not AppError
    *  Should just use the more generic wrap Throwable really.
    * @param ce
    * @return
    */
  def wrapBinaryCodecError(ce: RippleCodecError): AppError = new AppException("BinaryCodecError", ce)

  /** Catches thrown (non-fatal) exceptions from wrapped function */
  def wrap[A](msg: String)(fn: => Either[AppError, A]): Either[AppError, A] = {
    Try {
      fn
    } match {
      case Success(v: Either[AppError, A]) => v
      case Failure(exception)              => AppError(msg, exception).asLeft
    }
  }

  /** Catches thrown (non-fatal) exceptons and shift to Either.left*/
  def wrapPure[A](msg: String)(fn: => A): Either[AppError, A] = {
    Try {
      val res: A = fn
      res.asRight[AppError]
    } match {
      case Success(v)         => v
      case Failure(exception) => apply(msg, exception).asLeft
    }
  }

  val NOT_IMPLEMENTED_ERROR: Either[OError, Nothing] = AppError("Not Implemented").asLeft

  /** We can use this once we know its an AppError or subclass and not a Throwable */
  implicit val showAppError: Show[AppError] = Show.show[AppError] {
    case err: AppJsonError         => err.show
    case err: AppJsonDecodingError => err.show
    case err: AppException         => err.show
    case err: AppJsonParsingError  => err.show
    case err: OError               => "\n --- " + err.show
  }

}

object AppJsonError {
  implicit val showJsonErr: Show[AppJsonError] = Show.show { failure =>
    s"""
       | AppJsonError:\t ${failure.msg}
       | JSON :\t  ${failure.json.spaces2}
       | ${failure.cause.map(AppErrorUtils.showCause)}
    """.stripMargin
  }

}

object AppJsonParsingError {
  implicit val showJsonParsingErr: Show[AppJsonParsingError] = Show.show[AppJsonParsingError] {
    failure: AppJsonParsingError =>
      s"""
         |Parsing Error\t\t: ${failure.msg} \n"
         |Parse Error  \t\t: ${failure.parser.show}
         |On Txt:\n${failure.raw}
         |==
         | ${failure.cause.map(AppErrorUtils.showCause)}
         |""".stripMargin
  }
}

object AppJsonDecodingError {
  implicit val showJsonDecoding: Show[AppJsonDecodingError] = Show.show[AppJsonDecodingError] {
    failure: AppJsonDecodingError =>
      val base = s"ODecodingError -->  ${failure.cause.show} \n\t\t On JSON: ${failure.json.spaces2}"
      base + "\n DecodingFailure History: " + failure.cause.map(_.history)
  }
}

object AppException {

  implicit val showException: Show[AppException] = Show.show[AppException] { err =>
    s"OErrorException -->  ${err.msg} \n\t\t " +
      s"Exception Message: ${err.err}\n\t\t" +
      s"StackTrace As String: ${AppErrorUtils.stackAsString(err.err)}"

  }
}

object OError {

  /** Ignore the compimle error in IntelliJ, but not the crappy coding needs redo */
  implicit val showOError: Show[OError] = Show.show[OError] { failure: OError =>
    val msg = s"OError -> ${failure.msg}"

    val causedBy = failure.cause.map(AppErrorUtils.showCause)
    // Stack trace to come I guess.
    msg + causedBy
  }
}
