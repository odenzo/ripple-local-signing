package com.odenzo.ripple.localops

/**
  * This is the simple API to use the Ripple Local Operations. See RippleLocalOps for a superset of this API
  * that may be handy for existing code bases.
  */
trait RippleLocalAPI extends SecretKeyOps with RippleLocalOps {}

object RippleLocalAPI extends RippleLocalAPI
