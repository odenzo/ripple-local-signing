package com.odenzo.ripple.localops

import java.security.KeyPair

import io.circe.{Decoder, ObjectEncoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

object Models {

   type TxBlob = String


}


/** Allows for precalculation of Signing Key in implementation dependant format
  * This should be treated as an Opaque Type
  * For SECP keys this saves time deriving signing key from master_seed each time.
  */
trait SigningKey  {
  def signPubKey:String
}

case class SigningKeyEd25519(kp: AsymmetricCipherKeyPair, signPubKey:String) extends SigningKey
case class SigningKeySecp256(kp: KeyPair,signPubKey:String) extends SigningKey

case class Base58(v:String)
case class Base58Check(v:String)
case class TxnSignature(hex:String)


/*
 "error" : "invalidParams",
"error_code" : 31,
"error_message" : "Missing field 'secret'.",
 */
case class ResponseError(error: String, error_code: Option[Int], error_message: Option[String])

object ResponseError {
   def apply(err: String, code: Int, msg: String): ResponseError = ResponseError(err, Some(code), Some(msg))

   def invalid(msg: String): ResponseError = ResponseError("invalidParams", 31, msg)

   val kNoTxJson      = invalid("Missing field 'tx_json'")
   val kNoSecret      = invalid("Missing field 'secret'.")
   val kNoCommand     = ResponseError("missingCommand", None, None)
   val kBadCommand    = ResponseError("unknownCommand", None, Some("'sign' is the only command supporteds"))
   val kTooMany       = invalid("Exactly one of the following must be specified: passphrase, secret, seed or seed_hex")
   val kSecretAndType = invalid("The secret field is not allowed if key_type is used.")
   val kBadSecret     = ResponseError("badSecret", 41, "Secret does not match account.")
}


case class WalletProposeResult(
                        account_id: String,
                        key_type: String,
                        master_key: String,
                        master_seed:String,
                        master_seed_hex: String,
                        public_key: String,
                        public_key_hex: String
                      ) {

}


object WalletProposeResult {

   implicit val encoder: ObjectEncoder[WalletProposeResult] = deriveEncoder[WalletProposeResult]
   implicit val decoder: Decoder[WalletProposeResult]       = deriveDecoder[WalletProposeResult]

}
