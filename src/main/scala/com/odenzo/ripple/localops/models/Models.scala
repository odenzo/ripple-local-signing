package com.odenzo.ripple.localops.models

import java.security.KeyPair

import cats.implicits._
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

/** Allows for precalculation of Signing Key in implementation dependant format
  * This should be treated as an Opaque Type
  * For SECP keys this saves time deriving signing key from master_seed each time.
  */
sealed trait SigningKey {
  def signPubKey: String
}

case class SigningKeyEd25519(kp: AsymmetricCipherKeyPair, signPubKey: String) extends SigningKey
case class SigningKeySecp256(kp: KeyPair, signPubKey: String)                 extends SigningKey

/// Private below here, or not exported as part of standard interoperable stuff.

case class Base58(v: String)
case class Base58Check(v: String)
case class TxnSignature(hex: String)

/*
 "error" : "invalidParams",
"error_code" : 31,
"error_message" : "Missing field 'secret'.",
 */
case class ResponseError(error: String, error_code: Option[Int], error_message: Option[String])

object ResponseError {
  val kNoAccount     = invalid("Missing field 'account'")
  val kNoTxJson      = invalid("Missing field 'tx_json'")
  val kNoSecret      = invalid("Missing field 'secret'.")
  val kNoCommand     = ResponseError("missingCommand", None, None)
  val kBadCommand    = ResponseError("unknownCommand", None, Some("'sign' is the only command supporteds"))
  val kTooMany       = invalid("Exactly one of the following must be specified: passphrase, secret, seed or seed_hex")
  val kSecretAndType = invalid("The secret field is not allowed if key_type is used.")
  val kBadSecret     = ResponseError("badSecret", 41, "Secret does not match account.")

  def apply(err: String, code: Int, msg: String): ResponseError = ResponseError(err, Some(code), Some(msg))

  def invalid(msg: String): ResponseError = ResponseError("invalidParams", 31, msg)

  def internalErr(msg: String) = ResponseError("Internal Client System Error", 666.some, msg.some)
}
