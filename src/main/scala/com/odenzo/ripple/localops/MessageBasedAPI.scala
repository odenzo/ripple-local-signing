package com.odenzo.ripple.localops

import cats._
import cats.data._
import io.circe.{Json, JsonObject}
import scribe.Logging

import com.odenzo.ripple.localops.impl.messagehandlers.{SignForMsg, SignMsg, WalletProposeMsg}

/** These API are more or less drop-in replacements for calling the rippled server for signing and multisigning.
  * API is designed with minimal dependancies.
  * Currently just JsonObject from Circe but thinking of
  *  adding String based requests also.
  */
object MessageBasedAPI extends Logging {

  /**
    * Request and Response objects documented at  https://xrpl.org/sign.html
    * @param signRq Full Request
    * @return Either a success or error JSON Object per Response Format
    */
  def sign(signRq: JsonObject): JsonObject = {
    SignMsg.processSignRequest(signRq).fold(identity, identity)
  }

  /**
    * Documented at  https://xrpl.org/sign_for.html  (Error or Success)
    * @return SignForRs Note that hash and tx_blob not updated yet
    *
    */
  def signFor(signForRq: Json): JsonObject = {
    SignForMsg.signFor(signForRq).fold(identity, identity)
  }

  /**
    * In some cases you can sign_for tx_json with existing Signer fields, which aggregated.
    * If doing N seperate sign_for each with 1 or more Signer in result, then this
    * can be used to aggregate all the Signer(s) and return submit_multisigned request.
    * @param signed A list of signFor responses.
    *
    * @return a submit_multsigned request message. In exceptional cases error on the left.
    */
  def createSubmitMultiSignedRq(signed: List[JsonObject]): Either[Throwable, JsonObject] = {
    SignForMsg.createSubmitMultiSignedRq(signed)
  }

  /**
    *  Full message based interface per https://xrpl.org/wallet_propose.html
    *
    */
  def generateWallet(walletProposeRq: JsonObject): JsonObject = {
    WalletProposeMsg.propose(walletProposeRq).fold(identity, identity)
  }
}
