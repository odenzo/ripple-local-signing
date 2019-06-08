package com.odenzo.ripple.localops.crypto.core

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.security.{KeyFactory, PrivateKey, PublicKey, SecureRandom, Signature}
import java.util.Base64

import com.typesafe.scalalogging.StrictLogging
import org.bouncycastle.asn1.DEROctetString
import org.bouncycastle.asn1.edec.EdECObjectIdentifiers
import org.bouncycastle.asn1.pkcs.PrivateKeyInfo
import org.bouncycastle.asn1.x509.{AlgorithmIdentifier, SubjectPublicKeyInfo}
import org.bouncycastle.asn1.x9.X9ECParameters
import org.bouncycastle.crypto.digests.{SHA256Digest, SHA512Digest}
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.params.{AsymmetricKeyParameter, ECDomainParameters, Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import org.bouncycastle.crypto.signers.Ed25519Signer
import org.bouncycastle.crypto.{AsymmetricCipherKeyPair, Digest}
import org.bouncycastle.jce.provider.BouncyCastleProvider

import com.odenzo.ripple.localops.utils.ByteUtils
import com.odenzo.ripple.localops.utils.caterrors.{AppError, OError}

/**
  *
  *  There is no account family for ed25519
  * https://tools.ietf.org/html/draft-josefsson-eddsa-ed25519-03#section-5.2
  * */
object ED25519CryptoBC extends StrictLogging with ByteUtils {

  import java.security.{Provider, Security}

  Security.addProvider(new BouncyCastleProvider)
  val provider: Provider = Security.getProvider("BC")

  final val curveName           = "Ed25519"
  final val keyType             = "Ed25519"
  val algo: AlgorithmIdentifier = new AlgorithmIdentifier(EdECObjectIdentifiers.id_Ed25519)

  val curve: X9ECParameters            = CustomNamedCurves.getByName("curve25519")
  val order: BigInteger                = curve.getCurve.getOrder
  val domainParams: ECDomainParameters = new ECDomainParameters(curve.getCurve, curve.getG, curve.getN, curve.getH)

  logger.info(s"Curve Order: $order")

  /** Generate signature using Bouncy Castle Directly */
  def edSign(payload: Array[Byte], kp: AsymmetricCipherKeyPair): Array[Byte] = {
    val edSigner: Ed25519Signer = new Ed25519Signer()
    edSigner.init(true, kp.getPrivate)
    edSigner.update(payload, 0, payload.length)
    val signature: Array[Byte] = edSigner.generateSignature()
    signature
  }

  // 64 byte signatures are compressed versions, 64 bytes are output
  def edVerify(payload: Array[Byte], sig: Array[Byte], pubKey: Ed25519PublicKeyParameters): Boolean = {
    val edSigner: Ed25519Signer = new Ed25519Signer()
    edSigner.init(false, pubKey)
    edSigner.update(payload, 0, payload.length)
    edSigner.verifySignature(sig)
  }

  def nativeGenerateKeyPair(): AsymmetricCipherKeyPair = {
    val RANDOM: SecureRandom = new SecureRandom()
    val keygen               = new Ed25519KeyPairGenerator()
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
    hex2Bytes(seedHex).map(_.toArray).map { bytes ⇒
      val keyFactory                              = KeyFactory.getInstance("Ed25519")
      val uppedTo32Bytes                          = HashingOps.sha512Half(bytes)
      val privateKey: Ed25519PrivateKeyParameters = new Ed25519PrivateKeyParameters(uppedTo32Bytes.toArray, 0)
      val publicKey: Ed25519PublicKeyParameters   = privateKey.generatePublicKey()
      val kp                                      = new AsymmetricCipherKeyPair(publicKey, privateKey)
      kp
    }
  }

  /**
    *
    * @param pubParams This must be the public key, to save external casting take more generic type
    * @return   THe public key, 33 bytes with ED prefix like Ripple does it
    */
  def publicKey2Hex(pubParams: AsymmetricKeyParameter): Either[OError, String] = {
    val key = pubParams.asInstanceOf[Ed25519PublicKeyParameters]
    if (key.isPrivate) {
      Left(AppError("Expected PublicKey but was Private"))
    } else {
      Right("ED" + ByteUtils.bytes2hex(key.getEncoded))
    }

  }

  def signingPubKey2KeyParameter(pubKeyHex: String): Either[AppError, Ed25519PublicKeyParameters] = {
    hex2Bytes(pubKeyHex.drop(2)).map(b ⇒ new Ed25519PublicKeyParameters(b.toArray, 0))
  }

  /**
    *
    * @param priv
    */
  def privateKey2keypair(priv: Array[Byte]): AsymmetricCipherKeyPair = {
    val keyFactory                              = KeyFactory.getInstance("Ed25519")
    val uppedTo32Bytes                          = HashingOps.sha512Half(priv)
    val privateKey: Ed25519PrivateKeyParameters = new Ed25519PrivateKeyParameters(uppedTo32Bytes.toArray, 0)
    val publicKey: Ed25519PublicKeyParameters   = privateKey.generatePublicKey()
    val kp                                      = new AsymmetricCipherKeyPair(publicKey, privateKey)
    kp
  }

  def privateKey2publicKey(priv: Array[Byte]): Array[Byte] = {
    logger.info(s"Private Key Length: ${priv.length}")
    // If its 32 go on, else if 16 then up it by hashing
    val normalizedBytes: Array[Byte] = priv.length match {
      case 16    ⇒ HashingOps.sha512Half(priv).toArray
      case 32    ⇒ priv
      case other ⇒ throw new IllegalArgumentException(s"ED25519 Private Key Len 16 or 32 was $other")
    }

    val privateKey: Ed25519PrivateKeyParameters = new Ed25519PrivateKeyParameters(normalizedBytes, 0)
    val publicKey: Ed25519PublicKeyParameters   = privateKey.generatePublicKey()
    publicKey.getEncoded

  }

  def digests() = {
    val digest512: Digest       = org.bouncycastle.jcajce.provider.util.DigestFactory.getDigest("SHA512")
    val digest256: SHA256Digest = new SHA256Digest()
    val another: SHA512Digest   = new SHA512Digest()

  }
  def testCase() = {

    // Test case defined in https://tools.ietf.org/html/rfc8037
    val msg         = "eyJhbGciOiJFZERTQSJ9.RXhhbXBsZSBvZiBFZDI1NTE5IHNpZ25pbmc".getBytes(StandardCharsets.UTF_8)
    val expectedSig = "hgyY0il_MGCjP0JzlnLWG1PPOt7-09PGcvMg3AIbQR6dWbhijcNR4ki4iylGjg5BhVsPt9g7sVvpAr_MuM0KAg"

    // Both formatted as 32bit raw key values (x and d)
    val privateKeyBytes: Array[Byte] = Base64.getUrlDecoder.decode("nWGxne_9WmC6hEr0kuwsxERJxWl7MmkZcDusAxyuf2A")
    val publicKeyBytes: Array[Byte]  = Base64.getUrlDecoder.decode("11qYAYKxCrfVS_7TyWQHOg7hcvPapiMlrwIaaPcHURo")

    val keyFactory = KeyFactory.getInstance("Ed25519")

    // Wrap public key in ASN.1 format so we can use X509EncodedKeySpec to read it

    val pubKeyInfo: SubjectPublicKeyInfo = new SubjectPublicKeyInfo(algo, new DEROctetString(publicKeyBytes))
    val x509KeySpec: X509EncodedKeySpec  = new X509EncodedKeySpec(pubKeyInfo.getEncoded)
    val jcaPublicKey: PublicKey          = keyFactory.generatePublic(x509KeySpec)

    // Wrap private key in ASN.1 format so we can use
    val privKeyInfo   = new PrivateKeyInfo(algo, new DEROctetString(privateKeyBytes))
    val pkcs8KeySpec  = new PKCS8EncodedKeySpec(privKeyInfo.getEncoded)
    val jcaPrivateKey = keyFactory.generatePrivate(pkcs8KeySpec)

    // Generate new signature
    val dsa = Signature.getInstance("EdDSA") // Edwards digital signature algorithm
    dsa.initSign(jcaPrivateKey)
    dsa.update(msg, 0, msg.length)
    val signature: Array[Byte] = dsa.sign
    val actualSignature        = Base64.getUrlEncoder.encodeToString(signature).replace("=", "")

    logger.info("Expected signature: {}", expectedSig)
    logger.info("Actual signature  : {}", actualSignature)

  }

  //
  /**
    * Generate new signature  using JCA style
    *
    * @param msg
    * @param privateKey   JCA Private Key
    */
  def sign(msg: Array[Byte], privateKey: PrivateKey) = {
    val dsa = Signature.getInstance("EdDSA") // Edwards digital signature algorithm  EdDSA-SHA512?
    dsa.initSign(privateKey)
    dsa.update(msg, 0, msg.length)
    val signature: Array[Byte] = dsa.sign
    val actualSignature        = Base64.getUrlEncoder.encodeToString(signature).replace("=", "")
  }

//
//    def from128Seed(seedBytes: Array[Byte]): EDKeyPair =  { assert (seedBytes.length == 16)
//return from256Seed(HashUtils.halfSha512(seedBytes))
//}

}
