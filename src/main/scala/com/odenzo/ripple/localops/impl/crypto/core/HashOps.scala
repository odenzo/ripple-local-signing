package com.odenzo.ripple.localops.impl.crypto.core

import java.security.MessageDigest

import com.odenzo.ripple.localops.impl.utils.ByteUtils

/**
  * Collection of Hashing Operations.
  * Inputs need to go Java byte[], but outputs are wrapped to IndexedSeq instead of Array[Byte] to get immutable ds.
  * May switch to immutable.ArraySeq.unsafeWrapArray to save the array copy since no one else has a handle on the
  * returned bytes from the digester.
  */
trait HashOps {
  //  Security.addProvider(new BouncyCastleProvider)

  /** This is equivalent to Ripple SHA512Half, it does SHA512 and returns first 32 bytes */
  def sha512Half(bytes: Array[Byte]): IndexedSeq[Byte] = {
    val full: IndexedSeq[Byte] = sha512(bytes)
    full.take(32)
  }

  /**
    * Default Java SHA256, should be the same as BouncyCastle sha512BC function.
    *
    * @param bytes
    *
    * @return 64-byte SHA512 hash with no salt added.
    */
  def sha256(bytes: Array[Byte]): IndexedSeq[Byte] = {
    val array = MessageDigest.getInstance("SHA-256").digest(bytes)
    ByteUtils.unsafeArrayToIndexedSequence(array)
  }

  /**
    * Default Java SHA512, should be the same as BouncyCastle sha512BC function.
    *
    * @param bytes
    *
    * @return 64-byte SHA512 hash with no salt added.
    */
  def sha512(bytes: Array[Byte]): IndexedSeq[Byte] = {
    // toIndexedSeq will do a copy, enforcing immutable
    // unsafeArray  on  ArraySeq will not copy, and give the underlying mutable array
    // toArray also does a copy since Seq is immutable
    val array = MessageDigest.getInstance("SHA-512").digest(bytes)
    ByteUtils.unsafeArrayToIndexedSequence(array)
  }

  /**
    * RipeMD160 digest/hash.
    *
    * @param bytes
    *
    * @return
    */
  def ripemd160(bytes: Array[Byte]): IndexedSeq[Byte] = {
    val md                  = MessageDigest.getInstance("RIPEMD160")
    val digest: Array[Byte] = md.digest(bytes)
    ByteUtils.unsafeArrayToIndexedSequence(digest)
  }

}

object HashOps extends HashOps
