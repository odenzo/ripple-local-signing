package com.odenzo.ripple.localops

import scala.util.{Failure, Success, Try}

import io.circe.Decoder.Result
import io.circe.{DecodingFailure, Json}

import cats._
import cats.implicits._
import scribe.{Logger, Logging}

import com.odenzo.ripple.bincodec.BinCodecLibError
import com.odenzo.ripple.localops.ErrorHandling.ErrorOr

/**
  * Odd case because I have chosen not to have a common library.
  * Intention is for BinCodec to be stand-alone and used with other Signing libs as well as this.
  * Similarly this may not always use BinCodec (does for now)s
  */
sealed trait LocalOpsError extends Throwable {

  def msg: String
  def cause: Option[LocalOpsError]

}

case class LOpErr(msg: String = "No Message", cause: Option[LocalOpsError] = None) extends LocalOpsError

case class WrappedBinCodecErr(msg: String, err: BinCodecLibError) extends LocalOpsError {
  val cause = LOpException(s"BinCodec Err: $msg", err).some
}

/**
  * For use in Request/Response scenarios, or more generally when  a:Json => b:Json => transform(b) => answer
  * This can be used.
  * Prefer to use ORestCall when have source and response json at one place.
  */
case class LOpJsonErr(msg: String = "JsonError", json: Json, cause: Option[LocalOpsError] = None) extends LocalOpsError

/** This should be the only class to have Throwable inside, wrap all the rest */
case class LOpException(val msg: String = "Wrapping Root Exception", val err: Throwable) extends LocalOpsError {
  val cause: Option[LocalOpsError] = Option.empty
}

object LocalOpsError extends Logging with ErrorUtils {

  val NOT_IMPLEMENTED_ERROR: Either[LOpErr, Nothing] = LocalOpsError("Not Implemented").asLeft

  // Builders for consistency and hiding a bit of the ever chaning mess.

  def apply(msg: String, err: Throwable): LOpException           = LOpException(msg, err)
  def apply(msg: String, json: Json): LOpJsonErr                 = LOpJsonErr(msg, json)
  def apply(msg: String, json: Json, err: Throwable): LOpJsonErr = LOpJsonErr(msg, json, LOpException("", err).some)
  def apply(m: String): LOpErr                                   = LOpErr(m, None)

  /** We can use this once we know its an AppError or subclass and not a Throwable */
  implicit val show: Show[LocalOpsError] = Show.show[LocalOpsError] {
    case err: LOpJsonErr         => err.show
    case err: LOpException       => err.show
    case err: WrappedBinCodecErr => err.show
    case err: LOpErr             => err.show
  }

}

object WrappedBinCodecErr {
  implicit val show: Show[WrappedBinCodecErr] = Show.show { failure =>
    s"""
       | Wrapped BinCodec Lib Error:\t ${failure.msg}
       | BinCodec Error: ${failure.cause.show}
    """.stripMargin
  }

}
object LOpJsonErr {
  implicit val showJsonErr: Show[LOpJsonErr] = Show.show { failure =>
    s"""
       | LOpJsonError:\t ${failure.msg}
       | JSON :\t  ${failure.json.spaces2}
       | ${failure.cause.fold("STACK: \n" + ErrorUtils.stackAsString(failure))("CAUSE " + _.show)}

    """.stripMargin
  }

}

object LOpException {

  implicit val showThrowable: Show[Throwable] = Show.show[Throwable] {
    case err: LocalOpsError    => err.show
    case err: BinCodecLibError => err.show
    case t: Throwable          => "\n LoclaOps Throwable" + t.toString
  }

  implicit val show: Show[LOpException] = Show.show[LOpException] { err =>
    s"AppException -->  ${err.msg} \n\t\t " +
      s"Exception Message: ${err.err.show}\n\t\t" +
      s"StackTrace As String: ${ErrorUtils.stackAsString(err.err)}"

  }
}

object LOpErr {

  implicit val showOError: Show[LOpErr] = Show.show[LOpErr] { failure: LOpErr =>
    val msg      = s"LOpErr -> ${failure.msg}  \n"
    val causedBy = failure.cause.map((e: LocalOpsError) => e.show)

    msg + causedBy
  }
}

trait ErrorUtils {

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
  def wrapDecodingResult[T](v: Result[T], json: Json, note: String = "No Clues"): Either[LOpJsonErr, T] = {
    v.leftMap { err: DecodingFailure =>
      LOpJsonErr(note, json, LOpException("Decoding Err", err).some)
    }
  }

  /**
    *  RippleBindaryCodec error is from library and extends Throwable but not AppError
    *  Should just use the more generic wrap Throwable really.
    * @param ce
    * @return
    */
  def wrapBinaryCodecError(ce: BinCodecLibError): LocalOpsError = WrappedBinCodecErr("BinaryCodecError", ce)

  /** Catches thrown (non-fatal) exceptions from wrapped function */
  def wrap[A](msg: String)(fn: => Either[LocalOpsError, A]): Either[LocalOpsError, A] = {
    Try {
      fn
    } match {
      case Success(v: Either[LocalOpsError, A]) => v
      case Failure(exception)                   => LocalOpsError(msg, exception).asLeft
    }
  }

  /** Catches thrown (non-fatal) exceptons and shift to Either.left*/
  def wrapPure[A](msg: String)(fn: => A): Either[LocalOpsError, A] = {
    Try {
      val res: A = fn
      res.asRight[LocalOpsError]
    } match {
      case Success(v)         => v
      case Failure(exception) => LOpException(msg, exception).asLeft
    }
  }

  /** Slightly changes to allow .tap semantics */
  def dump[T](ee: Either[Throwable, T]): Option[String] = {
    import LOpException.showThrowable
    ee.swap.map(e => e.show).toOption
  }

  /**
    *
    * @param ee
    * @param msg
    * @param mylog
    */
  def log(ee: ErrorOr[_], msg: String = "Error: ", mylog: Logger = scribe.Logger.logger): Unit = {
    dump(ee) match {
      case None       => mylog.debug("No Errors Found")
      case Some(emsg) => mylog.error(s"$msg\t=> $emsg ")
    }
  }

  def throwableSummary(t: Throwable): List[String] = {
    Option(t.getCause) match {
      case None        => t.getLocalizedMessage :: Nil
      case Some(cause) => t.getLocalizedMessage :: throwableSummary(cause)
    }
  }
  def stackAsString(err: Throwable): String = {
    import java.io.{PrintWriter, StringWriter}
    val errors = new StringWriter
    err.printStackTrace(new PrintWriter(errors))
    errors.toString
  }

  def printStackTrace(e: Throwable): String = {
    e.getStackTrace.slice(3, 19).map(_.toString).mkString("\n\t", "\n\t", "\n== .... ==\n")
  }

}

object ErrorUtils extends ErrorUtils

object ErrorHandling {
  // Can we just make this a trait and add some helpers?
  type ErrorOr[A] = Either[Throwable, A]

}
