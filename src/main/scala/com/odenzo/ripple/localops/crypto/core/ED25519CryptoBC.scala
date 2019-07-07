package com.odenzo.ripple.localops.crypto.core

import java.math.BigInteger
import java.security.{KeyFactory, SecureRandom}

import cats._
import cats.data._
import cats.implicits._
import scribe.Logging
import org.bouncycastle.asn1.edec.EdECObjectIdentifiers
import org.bouncycastle.asn1.x509.AlgorithmIdentifier
import org.bouncycastle.asn1.x9.X9ECParameters
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params._
import org.bouncycastle.crypto.signers.Ed25519Signer
import org.bouncycastle.jce.provider.BouncyCastleProvider

import com.odenzo.ripple.localops.utils.ByteUtils
import com.odenzo.ripple.localops.utils.caterrors.{AppError, AppException, OError}

/**
  *
  *  There is no account family for ed25519
  * https://tools.ietf.org/html/draft-josefsson-eddsa-ed25519-03#section-5.2
  * */
object ED25519CryptoBC extends Logging with ByteUtils {

  import java.security.Security

  Security.addProvider(new BouncyCastleProvider)

  private val curve: X9ECParameters = CustomNamedCurves.getByName("curve25519")

  val order: BigInteger = curve.getCurve.getOrder

//  private val domainParams: ECDomainParameters =
//    new ECDomainParameters(curve.getCurve, curve.getG, curve.getN, curve.getH)

  /** Generate signature using Bouncy Castle Directly */
  def edSign(payload: Array[Byte], kp: AsymmetricCipherKeyPair): Either[AppError, Array[Byte]] = {
    AppException.wrapPure("ED25519 Sign") {
      val edSigner: Ed25519Signer = new Ed25519Signer()
      edSigner.init(true, kp.getPrivate)
      edSigner.update(payload, 0, payload.length)
      val signature: Array[Byte] = edSigner.generateSignature()
      signature
    }
  }

  // 64 byte signatures are compressed versions, 64 bytes are output
  def edVerify(payload: Array[Byte],
               sig: Array[Byte],
               pubKey: Ed25519PublicKeyParameters): Either[AppError, Boolean] = {
    AppException.wrapPure("ED25519 Verify") {
      val edSigner: Ed25519Signer = new Ed25519Signer()
      edSigner.init(false, pubKey)
      edSigner.update(payload, 0, payload.length)
      edSigner.verifySignature(sig)
    }
  }

  def nativeGenerateKeyPair(): AsymmetricCipherKeyPair = {
    val RANDOM: SecureRandom = new SecureRandom()
    val keygen: Ed25519KeyPairGenerator = new Ed25519KeyPairGenerator()
    keygen.init(new Ed25519KeyGenerationParameters(RANDOM))
    val keyPair = keygen.generateKeyPair()
    keyPair
  }

  /**
    *   Takes the Bytes of Seed and SHA512Half them to make the private key (D) value
    * Then generates the public key directly from PrivateKey, no AccountFamily mumbo jumbo.
    * @param seedHex
    * @return A KeyPair but not class related to general ECKeyPair or KeyPair
    */
  def seedHex2keypair(seedHex: String): Either[AppError, AsymmetricCipherKeyPair] = {
    hex2bytes(seedHex).map(_.toArray).map(privateKey2keypair)
  }

  /**
    *
    * @param pubParams This must be the public key, to save external casting take more generic type
    * @return   THe public key, 33 bytes with ED prefix like Ripple does it
    */
  def publicKey2Hex(pubParams: AsymmetricKeyParameter): Either[AppError, String] = {
    AppException.wrap("ed publickey to hex") {
      val key = pubParams.asInstanceOf[Ed25519PublicKeyParameters]
      if (key.isPrivate) {
        Left(AppError("Expected PublicKey but was Private"))
      } else {
        Right("ED" + ByteUtils.bytes2hex(key.getEncoded))
      }
    }
  }

  def signingPubKey2KeyParameter(pubKeyHex: String): Either[AppError, Ed25519PublicKeyParameters] = {
    hex2bytes(pubKeyHex.drop(2)).map(b ⇒ new Ed25519PublicKeyParameters(b.toArray, 0))
  }

  /**
    *
    * @param priv
    */
  def privateKey2keypair(priv: Array[Byte]): AsymmetricCipherKeyPair = {
    val uppedTo32Bytes                          = HashOps.sha512Half(priv)
    val privateKey: Ed25519PrivateKeyParameters = new Ed25519PrivateKeyParameters(uppedTo32Bytes.toArray, 0)
    val publicKey: Ed25519PublicKeyParameters   = privateKey.generatePublicKey()
    val kp                                      = new AsymmetricCipherKeyPair(publicKey, privateKey)
    kp
  }

}
