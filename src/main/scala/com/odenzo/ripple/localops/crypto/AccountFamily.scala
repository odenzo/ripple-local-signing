package com.odenzo.ripple.localops.crypto

import java.math.BigInteger
import java.security.KeyPair
import scala.annotation.tailrec
import scala.collection.immutable

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import spire.math.UInt

import com.odenzo.jjutils.RFC1751Java
import com.odenzo.ripple.localops.crypto.core.{ED25519CryptoBC, HashOps, Secp256K1CryptoBC}
import com.odenzo.ripple.localops.utils.caterrors.{AppError, AppException}
import com.odenzo.ripple.localops.utils.{ByteUtils, RBase58}

/**
  * For secp the basic idea if that a FamilyGenerator has a public and private key
  * f(seed) => FamilyGenerator (FGPK FamilyGeneratorKeyPair)
  * f(FGPK) =>
  *
  * This is only applicable to secp256k1 key types.
  */
trait AccountFamily extends StrictLogging with ByteUtils with HashOps {

  /** The order of secp256k1 is the max value */
  protected val maxSecp: BigInteger = Secp256K1CryptoBC.secp256k1Order
  protected val ZERO_KEY: Seq[Byte]           = Seq.fill(32)(0x00.toByte)
  protected val MAX_KEY                       = BigInt(maxSecp)


  protected case class FamilyGenerator(publicKey: Array[Byte], privateKey: Array[Byte], keytype: String)

  protected case class AccountKeyPair(publicKey: Array[Byte], privateKey: Array[Byte], keytype: String)

  protected case class FullAccount(seed: Array[Byte], keyPair: AccountKeyPair, address: String) {
    def seedAsHex: String = bytes2hex(seed)

    def publicKeyJex: String = bytes2hex(keyPair.publicKey)
  }

  
  /** Rebuild Account Keys from Master Seed  - How to determine what keytype? */
  def rebuildAccountKeyPairFromSeedHex(mastSeedHex: String): Either[AppError, KeyPair] = {
    for {
      bytes          <- hex2Bytes(mastSeedHex)
      generator      = seed2FamilyGenerator(bytes)
      accountKeys    = familygenerator2accountKeyPair(generator)
      accountKeyPair = Secp256K1CryptoBC.dToKeyPair(BigInt(1, accountKeys.privateKey).bigInteger)
    } yield accountKeyPair

  }

  /**
    * This only applies to secp256k1 keys, not ed25519
    * @param seed
    */
  protected def seed2FamilyGenerator(seed: Seq[Byte]): FamilyGenerator = {
    val ZERO_KEY: Seq[Byte] = Seq.fill(32)(0x00.toByte)
    val MAX_KEY             = BigInt(maxSecp)
    @tailrec
    def pass(seed: Seq[Byte], index: UInt): Seq[Byte] = {
      val indexBytes: List[Byte] = ByteUtils.uint2bytes(index)
      val appended: Seq[Byte]    = seed ++ indexBytes
      val hash512                = sha512(appended)
      val privateKey: Seq[Byte]  = hash512.take(32)

      if (!(privateKey == ZERO_KEY) || (bytes2bigint(privateKey.toArray) > MAX_KEY)) {
        hash512 // TODO: Review this code
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

  /** secp256k1 only  */
  protected def familygenerator2accountKeyPair(generator: FamilyGenerator): AccountKeyPair = {

    val index_number: UInt                      = UInt(0L) // Default value, pubkey can reference multiple accounts I guess?
    val index_number_bytes: immutable.Seq[Byte] = uint2bytes(index_number)

    def pass(pubkey: Array[Byte], counterI: UInt): Seq[Byte] = {
      logger.info(s"Seed2Generator PubKey Seed Len ${pubkey.length} Index $counterI")
      val counterBytes: List[Byte] = ByteUtils.uint2bytes(counterI)
      val appended: Seq[Byte]      = pubkey ++ index_number_bytes ++ counterBytes
      val additionalKey: Seq[Byte] = sha512(appended).take(32)

      val isZero = additionalKey.forall(_ == 0x00.toByte)
      val tooBig = bytes2bigint(additionalKey.toArray) > MAX_KEY
      if (!(isZero || tooBig)) {
        additionalKey
      } else {
        pass(pubkey, counterI + UInt(1))
      }
    }

    val additionalKey: Seq[Byte] = pass(generator.publicKey, UInt(0))

    val sum: BigInt            = ByteUtils.bytes2bigint(generator.privateKey) + ByteUtils.bytes2bigint(additionalKey.toArray)
    val accountPrivKey: BigInt = sum % MAX_KEY // The order of corresponding curve used for each keytype

    val accountPrivKeyBytes = bigint2bytes(accountPrivKey)
    val acctPubKey: Array[Byte] = generator.keytype match {
      case "secp256k1" ⇒ Secp256K1CryptoBC.privatekey2publickeySecp256k1(accountPrivKeyBytes)
      case other       => throw new IllegalArgumentException(s"Invalid Key Type: $other")
    }
    AccountKeyPair(acctPubKey, accountPrivKeyBytes, generator.keytype)
  }


}

object AccountFamily extends AccountFamily
