package com.odenzo.ripple.localops.crypto

import java.math.BigInteger
import java.security.KeyPair
import scala.collection.immutable

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import spire.math.UInt

import com.odenzo.jjutils.RFC1751Java
import com.odenzo.ripple.localops.crypto.core.{ED25519CryptoBC, HashingOps, Secp256K1CryptoBC}
import com.odenzo.ripple.localops.utils.caterrors.{AppError, AppException}
import com.odenzo.ripple.localops.utils.{ByteUtils, RBase58}

/**
  * For secp the basic idea if that a FamilyGenerator has a public and private key
  * f(seed) => FamilyGenerator (FGPK FamilyGeneratorKeyPair)
  * f(FGPK) =>
  *
  */
object AccountFamily extends StrictLogging with ByteUtils with HashingOps {

  /** The order of curve is the max value */
  protected val maxEd25519: BigInteger = ED25519CryptoBC.order

  /** The order of secp256k1 is the max value */
  protected val maxSecp: BigInteger = Secp256K1CryptoBC.secp256k1Order

  /** Rebuild Account Keys from Master Seed  - How to determine what keytype? */
  def rebuildAccountKeyPairFromSeedHex(mastSeedHex: String): Either[AppError, KeyPair] = {
    hex2Bytes(mastSeedHex).map(_.toArray).flatMap(rebuildAccountKeyPairFromSeed)
  }

  def rebuildAccountKeyPairFromSeed(seed: Array[Byte]): Either[AppError, KeyPair] = {
    val generator: FamilyGenerator  = AccountFamily.seed2FamilyGeneratorSecp(seed)
    val accountKeys: AccountKeyPair = AccountFamily.familygenerator2accountKeyPair(generator)
    val accountKeyPair: KeyPair     = Secp256K1CryptoBC.dToKeyPair(BigInt(1, accountKeys.privateKey).bigInteger)
    accountKeyPair.asRight
  }

  /** Rebuild Account Keys from Master Seed */
  def rebuildAccountKeyPairFromSeedB58(masterSeedB58: String): Either[AppError, KeyPair] = {
    val bytes: Either[AppError, Array[Byte]] = RBase58.decode(masterSeedB58).map(_.drop(1).dropRight(4))
    bytes.flatMap(rebuildAccountKeyPairFromSeed)

  }

  /** This is Base16 format - Doesn't matter key_type */
  def passphase2seed(password: String): Seq[Byte] = sha512BC(password.getBytes("UTF-8")).take(16)

  /**
    * I am not actually sure if this applies to both keys, or just older secp256k
    * secp256k only i think, and that tests ok.
    * FamilyGenerate (Root Account)
    *
    * @param seed
    */
  def seed2FamilyGeneratorSecp(seed: Seq[Byte]): FamilyGenerator = {

    val ZERO = 0x00.toByte

    def pass(seed: Seq[Byte], index: UInt): Seq[Byte] = {
      logger.info(s"Seed2Generator Seed Len ${seed.length} Index $index")
      logger.info(s"SeedHex: ${bytes2hex(seed.toArray)}")

      val indexBytes: List[Byte] = ByteUtils.uint2bytes(index)
      val appended: Seq[Byte]    = seed ++ indexBytes
      val sha512                 = HashingOps.sha512(appended)
      val privateKey: Seq[Byte]  = sha512.take(32)

      val isZero = privateKey.forall(_ == ZERO)
      val tooBig = bytes2bigint(privateKey.toArray) > maxSecp
      if (!(isZero || tooBig)) {
        sha512
      } else {
        pass(seed, index + UInt(1))
      }
    }

    val fullHash = pass(seed.toList, UInt(0))
    logger.debug(s"Full Hash/Seed: len ${fullHash.length} => ${bytes2hex(fullHash.toArray)}")

    val fgPrivateKey: Seq[Byte]  = fullHash.take(32)
    val fgPublicKey: Array[Byte] = Secp256K1CryptoBC.privatekey2publickeySecp256k1(fgPrivateKey)
    FamilyGenerator(fgPublicKey, fgPrivateKey.toArray, "secp256k1")
  }

  /** TODO: Remove ed25519 stuff  */
  def familygenerator2accountKeyPair(generator: FamilyGenerator): AccountKeyPair = {
    val index_number: UInt                      = UInt(0L) // Default value, pubkey can reference multiple accounts I guess?
    val index_number_bytes: immutable.Seq[Byte] = uint2bytes(index_number)

    val maxValue = generator.keytype match {
      case "secp256k1" ⇒ maxSecp
      case "ed25519"   ⇒ maxEd25519
      case other       => throw new IllegalArgumentException(s"Invalid Key Type: $other")
    }

    def pass(pubkey: Array[Byte], counterI: UInt): Seq[Byte] = {
      logger.info(s"Seed2Generator PubKey Seed Len ${pubkey.length} Index $counterI")
      val counterBytes: List[Byte] = ByteUtils.uint2bytes(counterI)
      val appended: Seq[Byte]      = pubkey ++ index_number_bytes ++ counterBytes
      val additionalKey: Seq[Byte] = sha512(appended).take(32)

      val isZero = additionalKey.forall(_ == 0x00.toByte)
      val tooBig = bytes2bigint(additionalKey.toArray) > maxValue
      if (!(isZero || tooBig)) {
        additionalKey
      } else {
        pass(pubkey, counterI + UInt(1))
      }
    }

    val additionalKey: Seq[Byte] = pass(generator.publicKey, UInt(0))

    val sum: BigInt            = ByteUtils.bytes2bigint(generator.privateKey) + ByteUtils.bytes2bigint(additionalKey.toArray)
    val accountPrivKey: BigInt = sum % maxValue // The order of corresponding curve used for each keytype

    val accountPrivKeyBytes = bigint2bytes(accountPrivKey)
    val acctPubKey: Array[Byte] = generator.keytype match {
      case "secp256k1" ⇒ Secp256K1CryptoBC.privatekey2publickeySecp256k1(accountPrivKeyBytes)
      case "ed25519"   ⇒ ED25519CryptoBC.privateKey2publicKey(accountPrivKeyBytes)
      case other       => throw new IllegalArgumentException(s"Invalid Key Type: $other")
    }
    AccountKeyPair(acctPubKey, accountPrivKeyBytes, generator.keytype)
  }

  /**
    * @param publicKey secp265k or ed25519 keys key, if ed25519 padded with 0xED
    *
    * @return Ripple Account Address Base58 encoded with leading r
    *
    */
  def accountpubkey2address(publicKey: Seq[Byte]): String = {
    assert(publicKey.length == 32 || publicKey.length == 33)

    // Should start with ED if 32 byte  Ed25519
    val sha: Seq[Byte]       = sha256BC(publicKey)
    val accountId: Seq[Byte] = ripemd160(sha)
    val payload: Seq[Byte]   = 0.toByte +: accountId

    val checksumHash1: Seq[Byte] = sha256BC(payload)
    val checksum                 = sha256BC(checksumHash1).take(4)

    val bytes: Seq[Byte] = payload ++ checksum
    val b58: String      = RBase58.encode(bytes)
    b58
  }

  /** This trims off the first *byte* and the last four checksum bytes from
    *   Base58 Ripple encoded things. Suitable for master seed and public key
    * @param rippleB58
    * @return
    */
  def convertRippleBase58toBytes(rippleB58: String): Either[AppError, Array[Byte]] = {
    AppException.wrap(s"Converting MasterSeed $rippleB58 to MasterSeedHex") {
      for {
        bytes   <- RBase58.decode(rippleB58)
        trimmed = bytes.drop(1).dropRight(4)
      } yield trimmed
    }
  }

  /**
    * Dropping the first byte (part of s prefix) and the last 4 checksum bytes
    *
    * @param masterSeedB58 as in the response from WalletPropose (s...).
    */
  def convertMasterSeedB582MasterSeedHex(masterSeedB58: String): Either[AppError, String] = {
    AppException.wrap(s"Converting MasterSeed $masterSeedB58 to MasterSeedHex") {
      convertRippleBase58toBytes(masterSeedB58).map(ByteUtils.bytes2hex(_))
    }
  }

  /**
    * Dropping the first byte (part of a prefix) and the last 4 checksum bytes
    *
    * @param pubKeyB58d as in the response from WalletPropose (s...).
    */
  def convertPublicKeyB582PublicKeyHex(pubKeyB58d: String): Either[AppError, String] = {
    AppException.wrap(s"Converting PubKey $pubKeyB58d to PublicKeyHex") {
      convertRippleBase58toBytes(pubKeyB58d).map(ByteUtils.bytes2hex(_))
    }
  }

  /**
    * This is a straight conversion, givern a master_seed it yields the Family KeyPair not account KeyPair
    *
    * @param masterSeedB58 This is the sXXXXXX style of seed to represent private key.
    *
    * @return
    */
  def convertMasterSeedB58ToKeyPair(masterSeedB58: String): Either[AppError, KeyPair] = {

    for {
      hex          ← convertMasterSeedB582MasterSeedHex(masterSeedB58)
      secretRandom <- hex2ulong(hex).map(_.toBigInt.bigInteger)
      kp           = Secp256K1CryptoBC.dToKeyPair(secretRandom)
    } yield kp
  }

  /**
    * Suitable for converted WalletPropose secretKey to master_seed_hex
    *
    * @param masterKeyRFC1751
    *
    * @return Pure format conversion from RFC1751 human words to hex seed
    */
  def convertMasterKey2masterSeedHex(masterKeyRFC1751: String): String = {
    RFC1751Java.getKeyFromEnglish(masterKeyRFC1751)
  }

  case class FamilyGenerator(publicKey: Array[Byte], privateKey: Array[Byte], keytype: String)

  case class AccountKeyPair(publicKey: Array[Byte], privateKey: Array[Byte], keytype: String)

  case class FullAccount(seed: Array[Byte], keyPair: AccountKeyPair, address: String) {
    def seedAsHex: String = bytes2hex(seed)

    def publicKeyJex: String = bytes2hex(keyPair.publicKey)
  }

  object FamilyGenerator {}
}
