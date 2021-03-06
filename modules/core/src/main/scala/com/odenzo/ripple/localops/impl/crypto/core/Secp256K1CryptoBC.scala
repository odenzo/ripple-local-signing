package com.odenzo.ripple.localops.impl.crypto.core

import java.math.BigInteger
import java.security._

import cats._
import cats.data._
import cats.implicits._
import org.bouncycastle.asn1.sec.SECNamedCurves
import org.bouncycastle.asn1.x9.X9ECParameters
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.params.{ECDomainParameters, ECPrivateKeyParameters, ECPublicKeyParameters}
import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
import org.bouncycastle.jcajce.provider.asymmetric.ec.{BCECPrivateKey, BCECPublicKey}
import org.bouncycastle.jce.ECNamedCurveTable
import org.bouncycastle.jce.interfaces.ECPublicKey
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.jce.spec.{ECNamedCurveParameterSpec, ECPrivateKeySpec, ECPublicKeySpec}
import org.bouncycastle.math.ec
import org.bouncycastle.math.ec.ECPoint
import scribe.Logging

import com.odenzo.ripple.localops.LocalOpsError
import com.odenzo.ripple.localops.impl.crypto.DERSignature
import com.odenzo.ripple.localops.impl.utils.ByteUtils

/**
  * This is focussed just on getting Secp256k1 txnscenarios and Verification Working.
  * TODO: Trim this down to used functions
  **/
object Secp256K1CryptoBC extends Logging with ByteUtils {

  Security.addProvider(new BouncyCastleProvider)
  private val providerName = "BC"
  val provider: Provider   = Security.getProvider(providerName)

  private val curveName      = "secp256k1"
  private val keyType        = "ECDSA"
  val params: X9ECParameters = SECNamedCurves.getByName(curveName)

  private val domainParams: ECDomainParameters =
    new ECDomainParameters(params.getCurve, params.getG, params.getN, params.getH)
  private val ecSpec: ECNamedCurveParameterSpec = ECNamedCurveTable.getParameterSpec(curveName)

  /** G is generator for secp256k1 curve.
    * @return
    */
  val secp256k1Generator: ECPoint = params.getG
  val secp256k1Order: BigInteger  = params.getN

  /** Canonical Public Interface. For now it takes a KeyPair for internal validation
    *
    * @param message This is the Hash value to sign, for Ripple the (HashPrefix ++ SerializedForSigning(tx_json))
    *                applied to SHA512Half (I am pretty sure)
    * */
  def verify(message: Array[Byte], sig: DERSignature, pubKey: PublicKey): Either[LocalOpsError, Boolean] = {
    LocalOpsError.handleM("SECP Verify") {
      pubKey match {
        case bcecKey: BCECPublicKey =>
          val signer                        = new ECDSASigner
          val pubPoint                      = bcecKey.getQ
          val params: ECPublicKeyParameters = new ECPublicKeyParameters(pubPoint, domainParams)
          signer.init(false, params)
          signer.verifySignature(message, sig.r.asBigInteger, sig.s.asBigInteger).asRight

        case other => LocalOpsError(s"Illegal Public Key Type: ${other.getClass}").asLeft
      }
    }
  }

  /** extract D, from the ripple-lib Java */
  def sign(hash: Array[Byte], secret: KeyPair): Either[LocalOpsError, DERSignature] = {
    LocalOpsError.handleM("SECP SIGN") {

      val kCalc: HMacDSAKCalculator = new HMacDSAKCalculator(new SHA256Digest)
      val signer                    = new ECDSASigner(kCalc)

      privateKey2D(secret.getPrivate).flatMap { d =>
        val privKeyParam = new ECPrivateKeyParameters(d, domainParams)
        signer.init(true, privKeyParam)
        val sigs               = signer.generateSignature(hash)
        val r: BigInteger      = sigs(0)
        val s: BigInteger      = sigs(1)
        val otherS: BigInteger = secp256k1Order.subtract(s)
        val finalS             = if (s.compareTo(otherS) === 1) otherS else s
        DERSignature.fromRandS(r, finalS)
      }
    }
  }

  /**
    *
    * @return Generates a JCA KeyPair for secp256k1 in its own packaging
    */
  def generateNewKeyPair(): KeyPair = {
    val kpg: KeyPairGenerator = KeyPairGenerator.getInstance(keyType, providerName)
    kpg.initialize(ecSpec, new SecureRandom())
    val pair: KeyPair = kpg.generateKeyPair
    pair
  }

  /**
    * This is used for the special FamilyGenerator way of doing things.
    *
    * @param privateKey 32 bytes that corresponds to D value (magnitude)
    * @param compress   How the public key is encoded
    *
    * @return Compressed bytes for the public key.
    */
  def privatekey2publickeySecp256k1(privateKey: Array[Byte], compress: Boolean = true): IndexedSeq[Byte] = {

    val domain: ECDomainParameters = new ECDomainParameters(params.getCurve, params.getG, params.getN, params.getH)

    // This is a positive number, so 1, and then a bigendian binary array of the number
    val bd: BigInteger         = BigInt(1, privateKey.toArray).bigInteger
    val q: ECPoint             = domain.getG.multiply(bd)
    val publicParams           = new ECPublicKeyParameters(q, domain)
    val publicKey: Array[Byte] = publicParams.getQ.getEncoded(compress)
    publicKey.toIndexedSeq
  }

  /**
    * This is used now because FamilyGenerator creates d value.
    *
    * @param d The SECP356k ECDSA Key as BigInteger It is the random value of private key really.
    *
    * @return d converted to public and private keypair.  Make compressed public key.
    **/
  def dToKeyPair(d: BigInteger): KeyPair = {
    val eckf: KeyFactory = KeyFactory.getInstance("EC", "BC")

    val privateKeySpec: ECPrivateKeySpec = new ECPrivateKeySpec(d, ecSpec)
    val exPrivateKey: PrivateKey         = eckf.generatePrivate(privateKeySpec)

    val q: ECPoint                     = domainParams.getG.multiply(d)
    val publicKeySpec: ECPublicKeySpec = new ECPublicKeySpec(q, ecSpec)
    val exPublicKey: PublicKey         = eckf.generatePublic(publicKeySpec)

    new KeyPair(exPublicKey, exPrivateKey)
  }

  /**
    * Public Keys with X only are compressed with added 0x02 or 0x03 as first byte and 32 byte X
    * Uncompressed Public Keys have 0x04 and 32 byte X and 32 byte Y
    *
    * @param compressKey The public key, how these bytes are am not sure. uncompressed with 02 or 03?
    *
    * @return
    */
  def decompressPublicKey(compressKey: Array[Byte]): PublicKey = {
    val point: ec.ECPoint              = ecSpec.getCurve.decodePoint(compressKey)
    val Q                              = point
    val eckf: KeyFactory               = KeyFactory.getInstance("EC", providerName)
    val publicKeySpec: ECPublicKeySpec = new ECPublicKeySpec(Q, ecSpec)
    val exPublicKey: PublicKey         = eckf.generatePublic(publicKeySpec)
    exPublicKey
  }

  /**
    * @return Gives the raw bytes of the public key, which are  preceeded by 02 or 03 in encoded form.
    * This is for sepck key only now.
    */
  def compressPublicKey(pub: PublicKey): IndexedSeq[Byte] = {
    pub match {
      case p: ECPublicKey =>
        p.getQ.getEncoded(true).toIndexedSeq
    }
  }

  def publicKey2hex(pub: PublicKey): String = bytes2hex(compressPublicKey(pub))

  /**
    * Extract D value from private key.
    *
    * @param priv instance of BCECPrivateKey
    *
    * @return The D value of the private key.
    */
  def privateKey2D(priv: PrivateKey): Either[LocalOpsError, BigInteger] = {
    priv match {
      case k: BCECPrivateKey => k.getD.asRight
      case other             => LocalOpsError(s"PrivateKey ${other.getClass} wasnt BCECPrivateKey").asLeft
    }

  }

}
