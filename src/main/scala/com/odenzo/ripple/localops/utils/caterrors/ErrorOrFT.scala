package com.odenzo.ripple.localops.utils.caterrors

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging

import com.odenzo.ripple.localops.utils.caterrors.CatsTransformers.{ErrorOr, ErrorOrFT}

object CatsTransformers extends  StrictLogging {

  // Can we just make this a trait and add some helpers?
  type ErrorOr[A] = Either[AppError, A]

  /** Shorthand for a Future ErrorOr ... prefer to standardize on ErrorOrFT instead */
  type ErrorOrF[A] = Future[ErrorOr[A]]

  /** Uses Cats EitherT monad transformat to wrap Future[Either[OError,A]
   *  Would like to make it so this is the inferred type by intellij
   */
  type ErrorOrFT[A] = EitherT[Future, AppError, A]

  // See Advanced Scala with Cats.
  // type ErrorOrOptFT[A] = OptionT[ErrorOrFT]

  /** Useful Cats EitherT when not using the standard OError error type */
  type FutureEither[A, B] = EitherT[Future, A, B]

}

trait CatsTransformerOps {
  def notImplemented(implicit ec: ExecutionContext): ErrorOrFT[Nothing] = {
    fromEither(OError("Not Implemented").asLeft)
  }

  def pure[B](b: B)(implicit ec: ExecutionContext): ErrorOrFT[B] = {
    EitherT.pure[Future, AppError](b)
  }

  def fromEither[B](b: Either[AppError, B])(implicit ec: ExecutionContext): ErrorOrFT[B] = {
    EitherT.fromEither[Future](b)
  }

  def fromError[B](e: AppError)(implicit ec: ExecutionContext): ErrorOrFT[B] = {
    fromEither[B](e.asLeft[B])
  }

  def fromOpt[B](
    b:   Option[B],
    msg: String    = "Optional value not present"
  )(implicit ec: ExecutionContext): ErrorOrFT[B] = {
    b match {
      case Some(v) ⇒ pure[B](v)
      case None    ⇒ fromError(OError(msg))
    }
  }

  /** Takes a future and catches all non fatal exceptions, returning ErrorFT with the exception wrapped in OError
   */
  def fromFuture[B](b: Future[B])(implicit ec: ExecutionContext): ErrorOrFT[B] = {
    //val lifted: Future[Either[BError, B]] = Either.catchNonFatal(b).leftMap(t => new OErrorException(err=t))
    // .sequence

    val caught: Either[Throwable, Future[B]] = Either.catchNonFatal(b)
    val massaged: Either[Future[AppException], Future[B]] = {
      caught.leftMap { thrown ⇒
        val ex = new AppException(err = thrown)
        val wrapped: Future[AppException] = Future.successful(ex)
        wrapped
      }
    }
    val a2: Future[Either[AppError, B]] = massaged.bisequence
    val answer: EitherT[Future, AppError, B] = EitherT(a2)
    answer
  }

}

object ErrorOr {
  def ok[B](b: B): ErrorOr[B] = b.asRight[AppError]: ErrorOr[B]
  def failed[B](a: AppError): ErrorOr[B] = a.asLeft
}

