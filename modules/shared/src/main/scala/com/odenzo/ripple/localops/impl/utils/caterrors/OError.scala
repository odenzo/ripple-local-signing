package com.odenzo.ripple.localops.impl.utils.caterrors

import scala.util.{Failure, Success, Try}

import cats._
import cats.implicits._
import io.circe.Decoder.Result
import io.circe.{DecodingFailure, Json, ParsingFailure}
import scribe.{Logger, Logging}

import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError
import com.odenzo.ripple.localops.impl.utils.caterrors.CatsTransformers.ErrorOr

/**
  * Base class that all errors (including OError) must extends directly or indirectly.
  * Not quite ready to move to case classes.
  */
sealed trait AppError extends Throwable { // Note this is not sealed and some shit defined elsewhere in the pasta bowl
  def msg: String

  def cause: Option[AppError]

  def asErrorOr[A]: ErrorOr[A] = this.asLeft[A]
}

object ShowHack {
  implicit val showBaseError: Show[AppError] = Show.show[AppError] {
    case err: AppJsonError         => err.show
    case err: AppJsonDecodingError => err.show
    case err: AppException         => err.show
    case err: OError               => "\n --- " + err.show
    case other                     => "\n ****** Unknown Error" + other.toString
  }
}

/**
  * Base Error is never instanciated, but the apply is up here as convenience
  * and delegates down. These will go away soon.
  */
object AppError extends Logging {

  type MonadAppError[F[_]] = MonadError[F, AppError]

  lazy implicit val show: Show[AppError] = Show.show[AppError] { failure: AppError =>
    nonImplShow(failure)
  }

  /** Ignore the compile error in IntelliJ due to recursive show definition
    * Also not the generic restriction/assumption for now that Throwable is bottom oh error stack.
    * */
  lazy implicit val showThrowables: Show[Throwable] = Show.show[Throwable] { t =>
    s"Showing Throwable ${t.getMessage}" + Option(t.getCause).map((v: Throwable) => v.toString).getOrElse("<No Cause>")
  }
  val NOT_IMPLEMENTED_ERROR: ErrorOr[Nothing] = Left(AppError("Not Implemented"))

  def nonImplShow(failure: AppError): String = {
    val base                   = ShowHack.showBaseError.show(failure)
    val nested: Option[String] = failure.cause.map(sub => ShowHack.showBaseError.show(sub))
    val cause                  = nested.getOrElse("\tNo Nested Cause")

    base + "\n\t" + cause
  }

  def dump(ee: ErrorOr[_]): Option[String] = {
    val formatted = ee.swap.map(e => AppError.nonImplShow(e)).toOption
    formatted
  }

  /**
    *
    * @param ee
    * @param msg
    * @param mylog
    */
  def log(ee: ErrorOr[_], msg: String = "Error: ", mylog: Logger = logger): Unit = {
    dump(ee) match {
      case None       => mylog.debug("No Errors Found")
      case Some(emsg) => mylog.error(s"$msg\t=> $emsg ")
    }
  }

  def apply(json: Json): AppError = AppJsonError("Invalid Json", json)

  def apply(m: String, json: Json): AppError = AppJsonError(m, json)

  def apply(m: String, json: Json, e: AppError): AppError = AppJsonError(m, json, Some(e))

  def apply(m: String): OError = new OError(m, None)

  def apply(m: String, ex: Throwable): AppException = new AppException(m, ex)

  def wrapCodecError(ce: RippleCodecError): AppError = new AppException("BinaryCodecError", ce)

  /**
    * Produces a list of strings summarizing the error, going down the stack.
    */
  def summary(err: AppError): List[String] = {
    val quick = err.cause match {
      case None         => ("Solo Error: " + err.msg) :: Nil
      case Some(nested) => ("Nested Errors: " + err.msg) :: summary(nested)
    }
    val detail: String = AppError.nonImplShow(err)

    quick :+ detail
  }

}

/** SHould move to sealed module level hierarchies? */
/** The general error handling class, use instead of Throwables in EitherT etc .
  * Preferred method is to use the helper functions in StdContext.
  * These claseses may be made private in the future.
  */
class OError(val msg: String = "No Message", val cause: Option[AppError] = None) extends AppError

object OError {

  /** Ignore the compimle error in IntelliJ, but not the crappy coding needs redo */
  implicit val showOError: Show[OError] = Show.show[OError] { failure: OError =>
    val top = s"OError -> ${failure.msg}"
    val sub = failure.cause.map((x: AppError) => x.show)
    top + sub
  }
}

/** This should be terminal node only */
class AppException(val msg: String = "Wrapping Root Exception", val err: Throwable) extends AppError {
  val cause: Option[AppError] = Option.empty
}

object AppException extends StackUtils {

  implicit val show: Show[AppException] = Show.show[AppException] { errorException =>
    s"OErrorException -->  ${errorException.msg} \n\t\t " +
      s"Exception Message: ${errorException.err}\n\t\t" +
      s"Exception Class: \t${errorException.err.getClass}\n\t\t" +
      s"StackTrace As String: ${stackAsString(errorException.err)}"

  }

  /** Catches thrown (non-fatal) exceptions from wrapped function */
  def wrap[A](msg: String)(fn: => Either[AppError, A]): Either[AppError, A] = {
    Try {
      fn
    } match {
      case Success(v: Either[AppError, A]) => v
      case Failure(exception)              => AppException(msg, exception).asLeft
    }
  }

  /** Catches thrown (non-fatal) exceptons */
  def wrapPure[A](msg: String)(fn: => A): Either[AppError, A] = {
    Try {
      val res: A = fn
      res.asRight[AppError]
    } match {
      case Success(v)         => v
      case Failure(exception) => AppException(msg, exception).asLeft
    }
  }

  def apply(msg: String, err: Throwable) = new AppException(msg, err)
}

/**
  * For use in Request/Response scenarios, or more generally when  a:Json => b:Json => transform(b) => answer
  * This can be used.
  * Prefer to use ORestCall when have source and response json at one place.
  */
class AppJsonError(val msg: String, val json: Json, val cause: Option[AppError] = None) extends AppError {}

class AppJsonParsingError(val msg: String, val raw: String, val parser: ParsingFailure) extends AppError {
  val cause: Option[AppError] = new AppException(parser.message, parser).some
}

/**
  * Represents a error in Circe Json decoding (Json => Model)
  *
  * @param json JSON that was input, generally the complete document in scope
  * @param err  The decoding failure from Circe.
  * @param note Informational message to enhance the exception, provides context.
  */
class AppJsonDecodingError(val json: Json, val err: DecodingFailure, val note: String = "") extends AppError {
  val msg: String             = note + ":" + err.message
  val base: String            = s"\n OR: ${err.show}"
  val cause: Option[AppError] = None

}

object AppJsonDecodingError {
  implicit val show: Show[AppJsonDecodingError] = Show.show[AppJsonDecodingError] { failure: AppJsonDecodingError =>
    val base          = s"ODecodingError -->  ${failure.err.show} \n\t\t On JSON: ${failure.json.spaces2}"
    val stackAsString = "\n\nStack as String: " + StackUtils.stackAsString(failure.err)
    // val stackTrace = "\n\nStack Trace " + StackUtils.printStackTrace(failure.err)
    base + "\n DecodingFailure History: " + failure.err.history + stackAsString
  }

  /**
    * Wrap the Decoding error if there was one, and return as Either
    *
    * @param v
    * @param json
    * @param note
    * @tparam T
    *
    * @return
    */
  def wrapResult[T](v: Result[T], json: Json, note: String = "No Clues"): ErrorOr[T] = {
    v.leftMap { err: DecodingFailure =>
      new AppJsonDecodingError(json, err, note)
    }
  }

}

object AppJsonError {

  implicit val show: Show[AppJsonError] = Show.show { failure =>
    s"""
       | OErrorJson:
       | Error:\t ${failure.msg}
       | JSON :\t  ${failure.json.spaces2}
       | CAUSE:\t\n ${failure.cause
         .map((x: AppError) => x.show)
         .getOrElse("<Nothing>")}""".stripMargin
  }

  def apply(msg: String, json: Json): AppJsonError = new AppJsonError(msg, json)

  def apply(msg: String, json: Json, cause: AppError): AppJsonError = new AppJsonError(msg, json, Some(cause))

  def apply(msg: String, json: Json, cause: Option[AppError] = None): AppJsonError = {
    new AppJsonError(msg, json, cause)
  }
}
