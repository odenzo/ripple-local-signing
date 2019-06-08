package com.odenzo.ripple.localops.impl.utils.caterrors

import cats.Show
import scribe.Logger

import com.odenzo.ripple.bincodec.utils.caterrors.ErrorOr.ErrorOr

/**
  * What to have some nice chopping to for nested stack traces.
  */
trait AppErrorUtils {

  implicit val showThrowable: Show[Throwable] = Show.show[Throwable] {
    case err: AppError => AppError.showAppError.show(err)
    case t: Throwable  => "\n ****** Throwable" + t.toString
  }

  def showCause(err: Throwable): String = {
    s"\nCaused By:\n" + showThrowable.show(err)
  }

  /** Slightly changes to allow .tap semantics */
  def dump[T](ee: Either[Throwable, T]): Option[String] = {
    ee.swap.map(e => showThrowable.show(e)).toOption
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

  /**
    * Produces a list of strings summarizing the error, going down the stack.
    */
  def summary(err: AppError): List[String] = {
    val quick = err.cause match {
      case None                    => ("Solo Error: " + err.msg) :: Nil
      case Some(nested: AppError)  => ("Nested Errors: " + err.msg) :: summary(nested)
      case Some(nested: Throwable) => ("Nested Errors: " + err.msg) :: throwableSummary(nested)
    }
    val detail: String = showThrowable.show(err)

    quick :+ detail
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

object AppErrorUtils extends AppErrorUtils
