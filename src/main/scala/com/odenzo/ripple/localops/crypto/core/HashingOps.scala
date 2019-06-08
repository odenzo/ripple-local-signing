package com.odenzo.ripple.localops.crypto.core

import java.security.{MessageDigest, Security}

import org.bouncycastle.jce.provider.BouncyCastleProvider

/**
*  Collection of Hashing Operations. Ideally these should be ScalaJS and Scala Native compatable in future.
  *  Now implementing using Java Security and BouncyCastle
  *  TODO: Think about returning array bytes instead of Seq.
  *  TODO: Benchmark to learn how to use jmh
  */
trait HashingOps {

  Security.addProvider(new BouncyCastleProvider)


  /**
    * BouncyCastle SHA512 implementation used.
    *
    * @param bytes
    *
    * @return 64-byte SHA512 hash with no salt added.
    */
  def sha512BC(bytes: Seq[Byte]): Seq[Byte] = {
    val md: MessageDigest = MessageDigest.getInstance("SHA-512", "BC")
    val byteArray: Array[Byte] = bytes.toArray
    val sha512Bytes: Array[Byte] = md.digest(byteArray)
    sha512Bytes.toSeq
  }

  /**
    * BouncyCastle SH256 NOT Ripple 256  or sha512Half
    *
    * @param bytes
    *
    * @return 64-byte SHA512 hash with no salt added.
    */
  def sha256BC(bytes: Seq[Byte]): Seq[Byte] = {
    val md = MessageDigest.getInstance("SHA-256", "BC")
    val byteArray: Array[Byte] = bytes.toArray
    val digest: Array[Byte] = md.digest(byteArray)
    digest.toSeq
  }

  /** This is equivalent to Ripple SHA512Half, it does SHA512 and returns first 32 bytes*/
  def sha256Ripple(bytes: Seq[Byte]): Seq[Byte] = {
    sha512(bytes).take(32)
  }

  /** This is equivalent to Ripple SHA512Half, it does SHA512 and returns first 32 bytes */
  def sha512Half(bytes: Seq[Byte]): Seq[Byte] = sha256Ripple(bytes)

  /**
    * Default Java SHA512, should be the same as BouncyCastle sha512BC function.
    *
    * @param bytes
    *
    * @return 64-byte SHA512 hash with no salt added.
    */
  def sha512(bytes: Seq[Byte]): Seq[Byte] = {
    val md = MessageDigest.getInstance("SHA-512")
    val byteArray: Array[Byte] = bytes.toArray
    val digest: Array[Byte] = md.digest(byteArray)
    digest.toSeq
  }

  /**
    * RipeMD160 digest/hash.
    * @param bytes
    *
    * @return
    */
  def ripemd160(bytes: Seq[Byte]): Seq[Byte] = {
    val md = MessageDigest.getInstance("RIPEMD160")
    val byteArray: Array[Byte] = bytes.toArray
    val digest: Array[Byte] = md.digest(byteArray)
    digest.toSeq
  }



}

object HashingOps extends HashingOps
