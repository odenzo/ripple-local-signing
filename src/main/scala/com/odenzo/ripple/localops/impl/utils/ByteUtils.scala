package com.odenzo.ripple.localops.impl.utils

import cats._
import cats.data._
import cats.implicits._
import scribe.Logging
import spire.math.{UByte, UInt, ULong}

import com.odenzo.ripple.localops.LocalOpsError

trait BinaryValue

/**
  * H  * ex representation of binary value. May be odd length.
  *
  * @param v Hex string, without 0x prefix.
  */
case class Hex(v: String) extends BinaryValue

trait ByteUtils extends Logging {

  /** This is used to signal intention for now, as not 2.12 backward compatable */
  def unsafeArrayToIndexedSequence(ar: Array[Byte]): IndexedSeq[Byte] = {
    // ArraySeq.unsafeWrapArray(ar)
    ar.toIndexedSeq

  }

  val bytezero: Byte = 0.toByte

  def zeroEvenPadHex(hex: String): String = {
    hex.length % 2 match {
      case 0 => hex
      case 1 => "0" + hex
    }
  }

  def hex2byte(hex: Char): Either[IllegalArgumentException, Int] = {
    if ('0' <= hex && hex <= '9') (hex - '0').asRight
    else if ('A' <= hex && hex <= 'F') (hex - 'A' + 10).asRight
    else if ('a' <= hex && hex <= 'f') (hex - 'a' + 10).asRight
    else new IllegalArgumentException(s"Illegal Hex Char $hex").asLeft

    //((Character.digit(input.charAt(i), 16) << 4) + Character.digit(input.charAt(i + 1), 16)).toByte
  }

  def hex2bytes(hex: String): Either[LocalOpsError, List[Byte]] = {
    // sliding changing in 2.13
    val even: String                             = zeroEvenPadHex(hex)
    val listOf2: List[String]                    = even.grouped(2).toList
    val bytes: Either[LocalOpsError, List[Byte]] = listOf2.traverse(hex2byte)
    bytes
  }

  /** For when we need to move to Array[Byte] (but want immutability) */
  def hex2byteArray(hex: String): Either[LocalOpsError, IndexedSeq[Byte]] = hex2bytes(hex).map(_.toIndexedSeq)

  def bigint2bytes(bi: BigInt): Array[Byte] = bi.toByteArray

  def bytes2bigint(a: Array[Byte]): BigInt = BigInt(1, a)

  /**
    * @param v Must be a one or two character hex string
    *
    * @return
    */
  def hex2byte(v: String): Either[LocalOpsError, Byte] = {
    LocalOpsError.wrap(s"$v hex to Byte") {
      java.lang.Short.parseShort(v, 16).toByte.asRight
    }
  }

  /** Returns String with exactly two uppercased Hex digits */
  @inline
  final def byte2hex(byte: Byte): String = "%02X".format(byte)

  def bytes2hex(bytes: Iterable[Byte]): String = {
    bytes.map(byte2hex).mkString
  }

  /**
    * @return Formats unsigned byte as two hex characters, padding on left as needed (lowercase btw)
    */
  def ubyte2hex(v: UByte): String = "%02X".format(v.toByte)

  /**
    * Takes an arbitrary length string and returns an listed of unsigned bytes
    * If the number of hex digits is odd, is padded with zero on left.
    */
  def hex2ubytes(v: String): Either[LocalOpsError, List[UByte]] = {
    zeroEvenPadHex(v).grouped(2).map(hex2ubyte).toList.sequence
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
    * Not for speed
    *
    * @param v Must be a one or two character hex string not enforced
    *
    * @return
    */
  def hex2ubyte(v: String): Either[LocalOpsError, UByte] = {
    hex2byte(v).map(UByte(_))
  }

  /** Quicky to take 16 hex chars and turn into ULong. Hex prefixed with 0x if missing */
  def hex2ulong(hex: String): Either[LocalOpsError, ULong] = {
    LocalOpsError.wrapPure(s"Parsing ULong from $hex") {
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
