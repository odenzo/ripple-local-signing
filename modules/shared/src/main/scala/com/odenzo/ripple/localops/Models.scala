package com.odenzo.ripple.localops

import java.security.KeyPair

import cats._
import cats.data._
import cats.implicits._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, ObjectEncoder}
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

/** TODO: Split between exposed Models and internal models */
sealed trait KeyType {
  val txt: String

}

object SECP256K1 extends KeyType {
  val txt = "secp256k1"
}
object ED25519 extends KeyType {
  val txt = "ed25519"
}

object KeyType {
  implicit val decoder: Decoder[KeyType] = Decoder[String].emap(s => fromText(s).leftMap(_.error))
  implicit val encoded: Encoder[KeyType] = Encoder[String].contramap[KeyType](kt => kt.txt)

  def fromText(s: String): Either[ResponseError, KeyType] = {
    s.toLowerCase() match {
      case ED25519.txt   => ED25519.asRight
      case SECP256K1.txt => SECP256K1.asRight
      case other         => ResponseError.invalid(s"Invalid Key Type: $other ").asLeft
    }
  }
}

/** Allows for precalculation of Signing Key in implementation dependant format
  * This should be treated as an Opaque Type
  * For SECP keys this saves time deriving signing key from master_seed each time.
  */
sealed trait SigningKey {
  def signPubKey: String
}

case class SigningKeyEd25519(kp: AsymmetricCipherKeyPair, signPubKey: String) extends SigningKey
case class SigningKeySecp256(kp: KeyPair, signPubKey: String)                 extends SigningKey

// This is currently public, but better to return as JsonObject IMHO.
case class WalletProposeResult(
    account_id: String,
    key_type: KeyType,
    master_key: String,
    master_seed: String,
    master_seed_hex: String,
    public_key: String,
    public_key_hex: String
)

object WalletProposeResult {

  implicit val encoder: Encoder.AsObject[WalletProposeResult] = deriveEncoder[WalletProposeResult]
  implicit val decoder: Decoder[WalletProposeResult]          = deriveDecoder[WalletProposeResult]

}

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
  val kNoTxJson      = invalid("Missing field 'tx_json'")
  val kNoSecret      = invalid("Missing field 'secret'.")
  val kNoCommand     = ResponseError("missingCommand", None, None)
  val kBadCommand    = ResponseError("unknownCommand", None, Some("'sign' is the only command supporteds"))
  val kTooMany       = invalid("Exactly one of the following must be specified: passphrase, secret, seed or seed_hex")
  val kSecretAndType = invalid("The secret field is not allowed if key_type is used.")
  val kBadSecret     = ResponseError("badSecret", 41, "Secret does not match account.")

  def apply(err: String, code: Int, msg: String): ResponseError = ResponseError(err, Some(code), Some(msg))

  def invalid(msg: String): ResponseError = ResponseError("invalidParams", 31, msg)
}
