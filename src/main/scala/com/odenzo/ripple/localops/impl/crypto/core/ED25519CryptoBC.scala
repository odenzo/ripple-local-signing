package com.odenzo.ripple.localops.impl.crypto.core

import java.math.BigInteger
import java.security.SecureRandom

import cats._
import cats.data._
import org.bouncycastle.asn1.x9.X9ECParameters
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params._
import org.bouncycastle.crypto.signers.Ed25519Signer
import org.bouncycastle.jce.provider.BouncyCastleProvider
import scribe.Logging

import com.odenzo.ripple.localops.impl.utils.ByteUtils
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError

/**
  *
  * There is no account family for ed25519
  * https://tools.ietf.org/html/draft-josefsson-eddsa-ed25519-03#section-5.2
  *
  **/
object ED25519CryptoBC extends Logging with ByteUtils {

  import java.security.Security

  Security.addProvider(new BouncyCastleProvider)
  private val curve: X9ECParameters = CustomNamedCurves.getByName("curve25519")

  val order: BigInteger = curve.getCurve.getOrder

  //  private val domainParams: ECDomainParameters =
  //    new ECDomainParameters(curve.getCurve, curve.getG, curve.getN, curve.getH)

  /** Generate signature using Bouncy Castle Directly */
  def sign(payload: Array[Byte], kp: AsymmetricCipherKeyPair): Either[AppError, IndexedSeq[Byte]] = {
    AppError.wrapPure("ED25519 Sign") {
      val edSigner: Ed25519Signer = new Ed25519Signer()
      edSigner.init(true, kp.getPrivate)
      edSigner.update(payload, 0, payload.length)
      val signature: Array[Byte] = edSigner.generateSignature()
      signature.toIndexedSeq
    }
  }

  // 64 byte signatures are compressed versions, 64 bytes are output
  def verify(payload: Array[Byte], sig: Array[Byte], pubKey: Ed25519PublicKeyParameters): Either[AppError, Boolean] = {
    AppError.wrapPure("ED25519 Verify") {
      val edSigner: Ed25519Signer = new Ed25519Signer()
      edSigner.init(false, pubKey)
      edSigner.update(payload, 0, payload.length)
      edSigner.verifySignature(sig)
    }
  }

  /** Generates a totally random keypair, but now no impl to get back to seed hex etc */
  def generateKeyPair(): AsymmetricCipherKeyPair = {
    val RANDOM: SecureRandom            = new SecureRandom()
    val keygen: Ed25519KeyPairGenerator = new Ed25519KeyPairGenerator()
    keygen.init(new Ed25519KeyGenerationParameters(RANDOM))
    val keyPair = keygen.generateKeyPair()
    keyPair
  }

  /**
    * Takes the Bytes of Seed and SHA512Half them to make the private key (D) value
    * Then generates the public key directly from PrivateKey, no AccountFamily mumbo jumbo.
    *
    * @param seedHex Typically the seed is 128-bits
    *
    * @return A KeyPair but not class related to general ECKeyPair or KeyPair
    */
  def generateKeyPairFromHex(seedHex: String): Either[AppError, AsymmetricCipherKeyPair] = {
    hex2byteArray(seedHex).map(is => generateKeyPairFromBytes(is.toArray))
  }

  /**
    * Then generates the public key directly from PrivateKey, no AccountFamily mumbo jumbo.
    * SHA512 so the length is priv is somewhat arbitrary, usually 128-bits
    *
    * @param priv Array is needed to pass into Hash function, not modified
    */
  def generateKeyPairFromBytes(priv: Array[Byte]): AsymmetricCipherKeyPair = {
    val uppedTo32Bytes: IndexedSeq[Byte]        = HashOps.sha512Half(priv)
    val privateKey: Ed25519PrivateKeyParameters = new Ed25519PrivateKeyParameters(uppedTo32Bytes.toArray, 0)
    val publicKey: Ed25519PublicKeyParameters   = privateKey.generatePublicKey()
    val kp                                      = new AsymmetricCipherKeyPair(publicKey, privateKey)
    kp
  }

  /**
    *
    * @param pubParams This must be the public key, to save external casting take more generic type
    *
    * @return THe public key, 33 bytes with ED prefix like Ripple does it
    */
  def publicKey2Hex(pubParams: AsymmetricKeyParameter): Either[AppError, String] = {
    AppError.wrap("ed publickey to hex") {
      val key = pubParams.asInstanceOf[Ed25519PublicKeyParameters]
      if (key.isPrivate) {
        Left(AppError("Expected PublicKey but was Private"))
      } else {
        Right("ED" + ByteUtils.bytes2hex(key.getEncoded))
      }
    }
  }

  def signingPubKey2KeyParameter(pubKeyHex: String): Either[AppError, Ed25519PublicKeyParameters] = {
    hex2bytes(pubKeyHex.drop(2)).map(b => new Ed25519PublicKeyParameters(b.toArray, 0))
  }

}