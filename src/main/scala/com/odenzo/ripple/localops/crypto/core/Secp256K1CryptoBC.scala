package com.odenzo.ripple.localops.crypto.core

import java.math.BigInteger
import java.security.interfaces.ECPrivateKey
import java.security.spec.{ECParameterSpec, PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.security.{KeyFactory, KeyPair, KeyPairGenerator, PrivateKey, PublicKey, SecureRandom}

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import org.bouncycastle.asn1.sec.SECNamedCurves
import org.bouncycastle.asn1.x9.X9ECParameters
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.params.{ECDomainParameters, ECPrivateKeyParameters, ECPublicKeyParameters}
import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
import org.bouncycastle.jcajce.provider.asymmetric.ec.{BCECPrivateKey, BCECPublicKey}
import org.bouncycastle.jce.ECNamedCurveTable
import org.bouncycastle.jce.interfaces.ECPublicKey
import org.bouncycastle.jce.spec.{ECNamedCurveParameterSpec, ECPrivateKeySpec, ECPublicKeySpec}
import org.bouncycastle.math.ec
import org.bouncycastle.math.ec.{ECFieldElement, ECPoint}

import com.odenzo.ripple.localops.crypto.DERSignature
import com.odenzo.ripple.localops.utils.ByteUtils
import com.odenzo.ripple.localops.utils.caterrors.{AppError, OError}

/**
  * This is focussed just on getting Secp256k1 Signing and Verification Working.
  **/
object Secp256K1CryptoBC extends StrictLogging with ByteUtils {

  final val sigType   = "SHA256withECDSA" // Is this correct?
  final val curveName = "secp256k1"
  final val keyType   = "ECDSA"
  final val provider  = "BC"

  val params: X9ECParameters            = SECNamedCurves.getByName(curveName)
  val domainParams: ECDomainParameters  = new ECDomainParameters(params.getCurve, params.getG, params.getN, params.getH)
  val ecSpec: ECNamedCurveParameterSpec = ECNamedCurveTable.getParameterSpec(curveName)

  /** G is generator for secp256k1 curve. A constant
    * Compress is: G = 02 79BE667E F9DCBBAC 55A06295 CE870B07 029BFCDB 2DCE28D9 59F2815B 16F81798
    * Uncompresssed: 04 79BE667E F9DCBBAC 55A06295 CE870B07 029BFCDB 2DCE28D9 59F2815B 16F81798 483ADA77 26A3C465 5DA4FBFC 0E1108A8 FD17B448 A6855419 9C47D08F FB10D4B8
    *
    * @return
    */
  val secp256k1Generator: ECPoint = params.getG
  val secp256k1Order: BigInteger  = params.getN

  /** Canonical Public Interface. For now it takes a KeyPair for internal validation
    *
    * @param message This is the Hash value to sign, for Ripple the (HashPrefix ++ SerializedForSigning(tx_json))
    *                applied to SHA512Half (I am pretty sure)
    **/
  def verify(message: Array[Byte], sig: DERSignature, pubKey: PublicKey): Boolean = {
    val bcecKey: BCECPublicKey = pubKey.asInstanceOf[BCECPublicKey]

    val signer = new ECDSASigner
    val pubPoint = bcecKey.getQ
    val params: ECPublicKeyParameters = new ECPublicKeyParameters(pubPoint, domainParams)
    signer.init(false, params)
    signer.verifySignature(message, sig.r.asBigInteger, sig.s.asBigInteger)

  }

  /** Currently using this, slightly painful to extract D, from the ripple-lib Java */
  def sign(hash: Array[Byte], secret: KeyPair): Either[AppError, DERSignature] = {
    val kCalc: HMacDSAKCalculator = new HMacDSAKCalculator(new SHA256Digest)
    val signer                    = new ECDSASigner(kCalc)

    privateKey2D(secret.getPrivate).flatMap { d ⇒
      val privKeyParam = new ECPrivateKeyParameters(d, domainParams)
      signer.init(true, privKeyParam)
      val sigs   = signer.generateSignature(hash)
      val r      = sigs(0)
      val s      = sigs(1)
      val otherS = secp256k1Order.subtract(s)
      val finalS = if (s.compareTo(otherS) == 1) otherS else s
      DERSignature.fromRandS(r, finalS)
    }

  }

 

  /**
    *
    *
    * @return Generates a JCA / BouncyCastle keypair
    *         for secp256k1 in its own packaging
    */
  def generateNewKeyPair(): KeyPair = {
    val kpg: KeyPairGenerator = KeyPairGenerator.getInstance(keyType, provider)
    kpg.initialize(ecSpec, new SecureRandom())
    val pair: KeyPair = kpg.generateKeyPair
    pair
  }

  
  /**
    * @param privateKey
    * @param compress
    *
    * @return Compressed bytes for the public key.
    */
  def privatekey2publickeySecp256k1(privateKey: Seq[Byte], compress: Boolean = true): Array[Byte] = {

    Class.forName("org.bouncycastle.asn1.sec.SECNamedCurves")
    val params: X9ECParameters     = SECNamedCurves.getByName(curveName)
    val domain: ECDomainParameters = new ECDomainParameters(params.getCurve, params.getG, params.getN, params.getH)

    // This is a positive number, so 1, and then a bigendian binary array of the number
    val bd: BigInteger         = BigInt(1, privateKey.toArray).bigInteger
    val q: ECPoint             = domain.getG.multiply(bd)
    val publicParams           = new ECPublicKeyParameters(q, domain)
    val publicKey: Array[Byte] = publicParams.getQ.getEncoded(true)
    publicKey
  }

  /**
    * @param d The SECP356k ECDSA Key as BigInteger It is the random value of private key really.
    *
    * @return d converted to public and private keypair.  Make compressed public key.
    **/
  def dToKeyPair(d: BigInteger): KeyPair = {
    val eckf: KeyFactory = KeyFactory.getInstance("EC", "BC")

    val privateKeySpec: ECPrivateKeySpec = new ECPrivateKeySpec(d, ecSpec)
    val exPrivateKey: PrivateKey         = eckf.generatePrivate(privateKeySpec)
    val p2: ECParameterSpec              = exPrivateKey.asInstanceOf[ECPrivateKey].getParams

    val q                                   = domainParams.getG.multiply(d)
    val publicParams: ECPublicKeyParameters = new ECPublicKeyParameters(q, domainParams)
    val Q: ec.ECPoint                       = publicParams.getQ

    val publicKeySpec: ECPublicKeySpec = new ECPublicKeySpec(Q, ecSpec)
    val exPublicKey: PublicKey         = eckf.generatePublic(publicKeySpec)
    val encoded: Array[Byte]           = exPrivateKey.getEncoded
    val privateEndoded                 = ByteUtils.bytes2hex(encoded)
    logger.info("Private Encoded: " + privateEndoded)

    new KeyPair(exPublicKey, exPrivateKey)
  }

  /**
    *
    * @param signingPubKeyHex SigningPublicKey in Hex format as appears in Sign Rq/Rs
    *
    * @return ECPoint with X and Y corresponding to Q. X should equal the pubkey value in message.
    */
  def signingPubKey2Q(signingPubKeyHex: String): Either[AppError, ECPoint] = {
    ByteUtils
      .hex2Bytes(signingPubKeyHex)
      .map(_.toArray)
      .map { bytes ⇒
        val point: ec.ECPoint = ecSpec.getCurve.decodePoint(bytes)
        val x: ECFieldElement = point.getXCoord
        val y: ECFieldElement = point.getYCoord

        val Q: ECPoint = point
        logger.info(s"SigningKey: $signingPubKeyHex")
        // Some Sanity Checking
        val decodedQx = x.toBigInteger
        val origQx: Either[AppError, BigInteger] =
          ByteUtils.hex2Bytes(signingPubKeyHex.drop(2)).map(v ⇒ new BigInteger(1, v.toArray))
        origQx.foreach(v ⇒ assert(v.compareTo(decodedQx) == 0))
        Q
      }

  }

  /**
    * Public Keys with X only are compressed with added 0x02 or 0x03 as first byte and 32 byte X
    * Uncompressed Public Keys have 0x04 and 32 byte X and 32 byte Y
    *
    * @param compressKey
    *
    * @return
    */
  def decompressPublicKey(compressKey: Array[Byte]): PublicKey = {
    val point: ec.ECPoint = ecSpec.getCurve.decodePoint(compressKey)
    val x: ECFieldElement = point.getXCoord
    val y: ECFieldElement = point.getYCoord

    val Q                              = point
    val eckf: KeyFactory               = KeyFactory.getInstance("EC", "BC")
    val publicKeySpec: ECPublicKeySpec = new ECPublicKeySpec(Q, ecSpec)
    val exPublicKey: PublicKey         = eckf.generatePublic(publicKeySpec)
    exPublicKey
  }

  /**
    * This takes a PublicKey that is really a BCECPublicKey from Bouncy Castle
    * and gets the X-Coordinate of Q value decoded. This is the format
    * used by Ripple for WalletRs public_key_hex adn signing public key
    *
    * @param publicKey
    */
  def publicKey2rawhex(publicKey: PublicKey): String = {
    val bcecKey: BCECPublicKey = publicKey.asInstanceOf[BCECPublicKey]
    bcecKey.setPointFormat("COMPRESSED")
    val compressed = ByteUtils.bytes2hex(bcecKey.getEncoded)
    val decoded    = compressed.drop(46)
    decoded
  }

  /** Return the Qx value for public key -- normaly uncompressed in KeyPair */
  def publicKey2Q(pub: PublicKey): ECPoint = {
    val bcecKey: BCECPublicKey = pub.asInstanceOf[BCECPublicKey]
    bcecKey.setPointFormat("COMPRESSED")
    bcecKey.getEncoded
    bcecKey.getQ
  }

  def publicKey2Qx(pub: PublicKey): BigInteger = {
    val bcecKey: BCECPublicKey = pub.asInstanceOf[BCECPublicKey]
    bcecKey.setPointFormat("COMPRESSED")
    bcecKey.getQ.getRawXCoord.toBigInteger // Affine matches z=1
  }

  /**
    * Basically reformatting an incompressed public key
    * 02 or 03 based on sign, so this is really a hack
    *
    * @return Get the compressed form of public key as Hex (SECP).
    **/
  def keypair2signingPubKey(kp: KeyPair): Either[AppError, String] = {
    val encoded: Array[Byte] = kp.getPublic.getEncoded // This is 04 form
    val hex                  = bytes2hex(encoded)
    logger.info(s"HEX Public Key Uncompressed: $hex")
    val compressed: String = "03" + hex.slice(48, 112)
    compressed.asRight
  }

  /**
    *
    * @param keyAsHex The encoded Key is Hex form, e.g. 03xxxxx from SigningPubKey
    *
    * @return
    */
  def decompressPublicKeyHex(keyAsHex: String): Either[AppError, PublicKey] = {
    ByteUtils.hex2Bytes(keyAsHex).map(v => decompressPublicKey(v.toArray))
  }

  /**
    * Extract D value from private key.
    *
    * @param priv
    *
    * @return
    */
  def privateKey2D(priv: PrivateKey): Either[AppError, BigInteger] = {
    logger.info("Private Key: " + priv.getClass)
    val key: Either[OError, BCECPrivateKey] = priv match {
      case k: BCECPrivateKey ⇒ k.asInstanceOf[BCECPrivateKey].asRight
      case other             ⇒ AppError(s"PrivateKey ${priv.getClass} wasnt BCECPrivateKey").asLeft
    }

    key.map(_.getD)
  }

  /**
    * Gives the raw bytes of the public key, which are normally preceeded by 02 in encoded form.
    * This is for sepck key only now.
    *
    * @param pub
    *
    * @return
    */
  def compressPublicKey(pub: PublicKey): Array[Byte] = {
    val params = pub match {
      case p: ECPublicKey => p.getParameters
    }
    pub.getEncoded.drop(24).take(32)
  }


  /**
    * Dangling Example of Encoding KeyPairs. Somewhere theres a way to specific public key is compressed or not.
    */
  def keyPairWrapping(pair: KeyPair): KeyPair = {
    val keyFactory = KeyFactory.getInstance(keyType, provider)

    val publicKeySpec = new X509EncodedKeySpec(pair.getPublic.getEncoded)
    val publicKey     = keyFactory.generatePublic(publicKeySpec)

    val privateKeySpec = new PKCS8EncodedKeySpec(pair.getPrivate.getEncoded)
    val privateKey     = keyFactory.generatePrivate(privateKeySpec)

    new KeyPair(publicKey, privateKey)
  }

}
