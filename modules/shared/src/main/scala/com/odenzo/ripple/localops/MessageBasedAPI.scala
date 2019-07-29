package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import scribe.Logging

import com.odenzo.ripple.localops.impl.messagehandlers.{SignForRqRsHandler, SignRqRsHandler}

/** These API are more or less drop-in replacements for calling the rippled server for signing and multisigning.
  * API is designed with minimal dependancies. Currently just JsonObject from Circe but thinking of
  * swithing over to String and parsing instead.
  * (Near term plan is to cross-compile to Scala Native and Scala JS and play with GraalVM work.
  * Also see if can get it to work in an iPhone app)
  *
  * */
object MessageBasedAPI extends Logging {

  /**
    *
    * @param signRq
    *
    * @return Either a success or error JSON Object similar to Ripple
    */
  def sign(signRq: JsonObject): JsonObject = {

    SignRqRsHandler.processSignRequest(signRq) match {
      case Left(v)  => v
      case Right(v) => v
    }

  }

  /**
    * Mimics a SignRq as much as possible. The SignRs is not returned, instead
    * just the TxBlob for use in the SubmitRq
    * Note that the Fee should already be specified, also all the paths.
    *
    * @return SignForRs as documented at  https://xrpl.org/sign_for.html
    *
    */
  def signFor(signForRq: JsonObject): JsonObject = {

    SignForRqRsHandler.signFor(signForRq) match {
      case Left(v)  => v
      case Right(v) => v
    }

  }

  /**
    * This takes a list of seperate signFor and combined the Signers from each, sorts and prepares a submit request.
    * In most cases each signFor response will have one Signer, but it handles 1+ Signer in each request.
    * You may want to supplement/update the message replacing id and/or fail_hard
    *
    * @param signed A list of signFor responses.
    *
    * @return a submit_multsigned request message. In exceptional cases error on the left.
    */
  def createSubmitMultiSignedRq(signed: List[JsonObject]): Either[Throwable, JsonObject] = {

    JsonObject.empty.asRight

  }

  /**
    *
    */
  def generateWallet(walletProposeRq: JsonObject) = {
    JsonObject.empty
  }
}
