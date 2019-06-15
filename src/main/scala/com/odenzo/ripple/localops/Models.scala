package com.odenzo.ripple.localops

import java.security.KeyPair

import org.bouncycastle.crypto.AsymmetricCipherKeyPair

object Models {

   type TxBlob = String


}


/** Allows for precalculation of Signing Key in implementation dependant format
  * This should be treated as an Opaque Type
  * For SECP keys this saves time deriving signing key from master_seed each time.
  */
trait SigningKey

case class SigningKeyEd25519(kp: AsymmetricCipherKeyPair) extends SigningKey

case class SigningKeySecp256(kp: KeyPair) extends SigningKey

case class Base58Check(b58:String)

case class TxnSignature(hex:String)
