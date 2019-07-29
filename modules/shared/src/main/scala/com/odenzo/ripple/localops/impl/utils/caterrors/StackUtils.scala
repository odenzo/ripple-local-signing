package com.odenzo.ripple.localops.impl.utils.caterrors

/**
  * What to have some nice chopping to for nested stack traces.
  */
trait StackUtils {

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

object StackUtils extends StackUtils
