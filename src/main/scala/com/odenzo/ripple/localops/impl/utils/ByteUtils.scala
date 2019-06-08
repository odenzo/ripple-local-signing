package com.odenzo.ripple.localops.impl.utils

import cats._
import cats.data._
import cats.implicits._
import scribe.Logging
import spire.math.{UByte, UInt, ULong}

import com.odenzo.ripple.localops.impl.utils.caterrors.AppError

trait BinaryValue

/**
  * H  * ex representation of binary value. May be odd length.
  *
  * @param v Hex string, without 0x prefix.
  */
case class Hex(v: String) extends BinaryValue

trait ByteUtils extends Logging {

  /**
    * Java routine inputs need to go Java byte[], but outputs are wrapped to IndexedSeq instead of Array[Byte] to get
    * immutable data structure. If the Array[Byte] has transient, then can use to immutable.ArraySeq.unsafeWrapArray to
    * save the array copy since no one else has a handle
    *
    * In cases were the transient Array spans two functions we use this to avoid copying of Array.
    * Note: It is the responsibility of the fna and fnb to ensure that the indexed sequence returned is safe, and
    * underlying data is not shared by a mutable Array that some rougue may have a handle to.
    *
    * @param a
    * @param b
    *
    * @return
    */
  def unsafeCompose(
      a: Array[Byte] => IndexedSeq[Byte],
      b: Array[Byte] => IndexedSeq[Byte]
  ): Array[Byte] => IndexedSeq[Byte] = {
    // Woops, more coffe.
    // First a standard super safe copy until testing is done
    // Then replace with ArraySeq.unsafe
    a andThen indexedSequenceToArray andThen b
  }

  /** This will copy the data to a new array  */
  def indexedSequenceToArray(is: IndexedSeq[Byte]): Array[Byte] = is.toArray

  /** This is used to signal intention for now, as not 2.12 backward compatable */
  def unsafeArrayToIndexedSequence(ar: Array[Byte]): IndexedSeq[Byte] = {

    // ArraySeq.unsafeWrapArray(ar)
    ar.toIndexedSeq

  }

  val bytezero: Byte = 0.toByte

  def zeroPadLeft(v: String, len: Int): String = {
    val maxPad: String = "000000000000000000000000000000000000000000000000000000000000000000"
    len - v.length match {
      case c if c > 0             => maxPad.take(c) + v
      case c if c === 0           => v
      case c if c > maxPad.length => zeroPadLeft(maxPad + v, len)
    }
  }

  def zeroEvenPadHex(hex: String): String = {
    hex.length % 2 match {
      case 0 => hex
      case 1 => "0" + hex
    }
  }

  def ensureMaxLength[T](l: List[T], len: Int): Either[AppError, List[T]] = {
    if (l.length > len) AppError(s"List too long.. ${l.length} > $len").asLeft
    else l.asRight
  }

  def hex2bytes(hex: String): Either[AppError, List[Byte]] = {
    // sliding changing in 2.13
    val even: String                        = zeroEvenPadHex(hex)
    val listOf2: List[String]               = even.grouped(2).toList
    val bytes: Either[AppError, List[Byte]] = listOf2.traverse(hex2byte)
    bytes
  }

  /** For when we need to move to Array[Byte] (but want immutability) */
  def hex2byteArray(hex: String): Either[AppError, IndexedSeq[Byte]] = hex2bytes(hex).map(_.toIndexedSeq)

  def bigint2bytes(bi: BigInt): Array[Byte] = bi.toByteArray

  def bytes2bigint(a: Array[Byte]): BigInt = BigInt(1, a)

  /**
    * @param v Must be a one or two character hex string
    *
    * @return
    */
  def hex2byte(v: String): Either[AppError, Byte] = {
    AppError.wrap(s"$v hex to Byte") {
      java.lang.Short.parseShort(v, 16).toByte.asRight
    }
  }

  /** Returns String with exactly two uppercased Hex digits */
  @inline
  final def byte2hex(byte: Byte): String = "%02X".format(byte)

  def bytes2hex(bytes: Iterable[Byte]): String = bytes.map(byte2hex).mkString

  /**
    * @return Formats unsigned byte as two hex characters, padding on left as needed (lowercase btw)
    */
  def ubyte2hex(v: UByte): String = "%02X".format(v.toByte)

  def byte2ubyte(b: Byte): UByte = UByte(b)

  def ubytes2hex(v: Seq[UByte]): String = v.map(ubyte2hex).mkString

  /**
    * Takes an arbitrary length string and returns an listed of unsigned bytes
    * If the number of hex digits is odd, is padded with zero on left.
    */
  def hex2ubytes(v: String): Either[AppError, List[UByte]] = {
    val padded: String = v.length % 2 match {
      case 0 => v
      case 1 => '0' +: v
    }
    padded.grouped(2).map(hex2ubyte).toList.sequence
  }

  /**
    * Unsafe converstion of Hex to list of Unsigned Bytes.
    * If hex is invalid then it throw Exception
    *
    * @param v
    *
    * @return
    */
  def unsafeHex2ubytes(v: String): List[UByte] = {
    hex2ubytes(v) match {
      case Right(list) => list
      case Left(err)   => throw new Exception(s"Programming Error $err")
    }
  }

  /**
    * Note for speed
    *
    * @param v Must be a one or two character hex string not enforced
    *
    * @return
    */
  def hex2ubyte(v: String): Either[AppError, UByte] = {
    hex2byte(v).map(UByte(_))
  }

  /** Quicky to take 16 hex chars and turn into ULong. Hex prefixed with 0x if missing */
  def hex2ulong(hex: String): Either[AppError, ULong] = {
    AppError.wrapPure(s"Parsing ULong from $hex") {
      ULong(java.lang.Long.parseUnsignedLong(hex, 16))
    }
  }

  def uint2bytes(v: UInt): List[Byte] = {
    val mask = UInt(255)
    List(
      (mask & v).toByte,
      (mask & (v >> 8)).toByte,
      (mask & (v >> 16)).toByte,
      (mask & (v >> 24)).toByte
    )
  }
}

object ByteUtils extends ByteUtils
