package com.odenzo.ripple.localops.impl.crypto

import java.math.BigInteger
import java.security.KeyPair
import scala.annotation.tailrec
import scala.collection.immutable

import cats._
import cats.data._
import cats.implicits._
import scribe.Logging
import spire.math.UInt

import com.odenzo.ripple.localops.LocalOpsError
import com.odenzo.ripple.localops.impl.crypto.core.{HashOps, Secp256K1CryptoBC}
import com.odenzo.ripple.localops.impl.utils.ByteUtils

/**
  * For secp the basic idea if that a FamilyGenerator has a public and private key
  * f(seed) => FamilyGenerator (FGPK FamilyGeneratorKeyPair)
  * f(FGPK) =>
  *
  * This is only applicable to secp256k1 key types.
  */
trait AccountFamily extends Logging with ByteUtils with HashOps {

  /** The order of secp256k1 is the max value */
  protected val maxSecp: BigInteger = Secp256K1CryptoBC.secp256k1Order
  protected val ZERO_KEY: Seq[Byte] = Seq.fill(32)(0x00.toByte)
  protected val MAX_KEY             = BigInt(maxSecp)

  /** Rebuild Account Keys from Master Seed Hex (Always Secp) */
  def rebuildAccountKeyPairFromSeedHex(mastSeedHex: String): Either[LocalOpsError, KeyPair] = {
    for {
      bytes          <- hex2byteArray(mastSeedHex)
      accountKeyPair <- rebuildAccountKeyPairFromSeed(bytes)
    } yield accountKeyPair

  }

  /** Rebuild Account Keys from Master Seed Binary */
  def rebuildAccountKeyPairFromSeed(seed: IndexedSeq[Byte]): Either[LocalOpsError, KeyPair] = {
    LocalOpsError.handle("Building secp Account KeyPair from Seed") {
      val generator      = seed2FamilyGenerator(seed)
      val d              = familygenerator2accountKeyPair(generator)
      val accountKeyPair = Secp256K1CryptoBC.dToKeyPair(d)
      accountKeyPair
    }
  }

  /**
    * This only applies to secp256k1 keys, not ed25519
    *
    * @param seed
    */
  protected def seed2FamilyGenerator(seed: Seq[Byte]): FamilyGenerator = {
    val ZERO_KEY: Seq[Byte] = Seq.fill(32)(0x00.toByte)
    val MAX_KEY             = BigInt(maxSecp)

    @tailrec
    def pass(seed: Seq[Byte], index: UInt): Seq[Byte] = {
      val indexBytes: List[Byte] = ByteUtils.uint2bytes(index)
      val appended: Seq[Byte]    = seed ++ indexBytes
      val hash512                = sha512(appended.toArray)
      val privateKey: Seq[Byte]  = hash512.take(32)

      // If ZERO or > MAX_KEY  loop else done
      privateKey.forall(_ === ByteUtils.bytezero) || (bytes2bigint(privateKey.toArray) > MAX_KEY) match {
        case false => hash512
        case true  => pass(seed, index + UInt(1))
      }
    }

    val fullHash = pass(seed.toList, UInt(0))
    logger.debug(s"Full Hash/Seed: len ${fullHash.length} => ${bytes2hex(fullHash)}")

    val fgPrivateKey: Seq[Byte] = fullHash.take(32)
    val fgPublicKey: IndexedSeq[Byte] =
      Secp256K1CryptoBC.privatekey2publickeySecp256k1(fgPrivateKey.toArray, compress = true)
    FamilyGenerator(fgPublicKey.toArray, fgPrivateKey.toArray)
  }

  /**
    * @return The D value of the account private key
    */
  protected def familygenerator2accountKeyPair(generator: FamilyGenerator): BigInteger = {

    val index_number: UInt                      = UInt(0L)
    val index_number_bytes: immutable.Seq[Byte] = uint2bytes(index_number)

    @tailrec
    def pass(pubkey: Seq[Byte], counterI: UInt): Seq[Byte] = {
      logger.debug(s"Seed2Generator PubKey Seed Len ${pubkey.length} Index $counterI")
      val counterBytes: List[Byte]        = ByteUtils.uint2bytes(counterI)
      val appended: Seq[Byte]             = pubkey ++ index_number_bytes ++ counterBytes.toIndexedSeq
      val additionalKey: IndexedSeq[Byte] = sha512(appended.toArray).take(32)

      val isZero = additionalKey.forall(_ === 0x00.toByte)
      val tooBig = bytes2bigint(additionalKey.toArray) > MAX_KEY
      if (!(isZero || tooBig)) {
        additionalKey
      } else {
        pass(pubkey, counterI + UInt(1))
      }
    }

    val additionalKey: Seq[Byte] = pass(generator.publicKey.toIndexedSeq, UInt(0))
    val sum: BigInt              = bytes2bigint(generator.privateKey) + bytes2bigint(additionalKey.toArray)
    val accountPrivKey: BigInt   = sum % MAX_KEY // The order of corresponding curve used for each keytype
    accountPrivKey.bigInteger
  }

  protected case class FamilyGenerator(publicKey: Array[Byte], privateKey: Array[Byte])

}

object AccountFamily extends AccountFamily
