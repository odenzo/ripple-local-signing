package com.odenzo.ripple.localops.impl.utils.caterrors

object ErrorHandling {
  // Can we just make this a trait and add some helpers?
  type ErrorOr[A] = Either[Throwable, A]

}
