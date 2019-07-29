package com.odenzo.ripple.localops.impl.crypto.core

import java.security.MessageDigest

/**
  * Collection of Hashing Operations. Ideally these should be ScalaJS and Scala Native compatable in future.
  * Now implementing using Java Security and BouncyCastle
  * TODO: Think about returning array bytes instead of Seq.
  * TODO: Benchmark to learn how to use jmh
  */
trait HashOps {
  //  Security.addProvider(new BouncyCastleProvider)

  /** This is equivalent to Ripple SHA512Half, it does SHA512 and returns first 32 bytes */
  def sha512Half(bytes: Seq[Byte]): IndexedSeq[Byte] = sha512(bytes).take(32).toIndexedSeq

  /**
    * Default Java SHA256, should be the same as BouncyCastle sha512BC function.
    *
    * @param bytes
    *
    * @return 64-byte SHA512 hash with no salt added.
    */
  def sha256(bytes: Seq[Byte]): IndexedSeq[Byte] = {
    MessageDigest.getInstance("SHA-256").digest(bytes.toArray).toIndexedSeq
  }

  /**
    * Default Java SHA512, should be the same as BouncyCastle sha512BC function.
    *
    * @param bytes
    *
    * @return 64-byte SHA512 hash with no salt added.
    */
  def sha512(bytes: Seq[Byte]): IndexedSeq[Byte] = {
    MessageDigest.getInstance("SHA-512").digest(bytes.toArray).toIndexedSeq
  }

  /**
    * RipeMD160 digest/hash.
    *
    * @param bytes
    *
    * @return
    */
  def ripemd160(bytes: Seq[Byte]): IndexedSeq[Byte] = {
    val md                  = MessageDigest.getInstance("RIPEMD160")
    val digest: Array[Byte] = md.digest(bytes.toArray)
    digest.toIndexedSeq
  }

}

object HashOps extends HashOps
