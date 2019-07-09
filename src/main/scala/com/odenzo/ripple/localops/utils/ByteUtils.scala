package com.odenzo.ripple.localops.utils

import java.util.Locale

import cats._
import cats.data._
import cats.implicits._
import scribe.Logging
import spire.implicits.bitStringOps
import spire.math.{UByte, UInt, ULong}

import com.odenzo.ripple.localops.utils.caterrors.{AppError, AppException, OError}


trait BinaryValue
/**
  * Hex representation of binary value. May be odd length.

  * @param v Hex string, without 0x prefix.
  */
case class Hex(v: String) extends BinaryValue


trait ByteUtils extends Logging {

  val bytezero: Byte = 0.toByte

  def hex2bytes(hex: String): Either[AppError, List[Byte]] = Nested(hex2ubytes(hex)).map(_.toByte).value

  def bigint2bytes(bi: BigInt): Array[Byte] = {
    val bytes: Array[Byte] = bi.toByteArray // Not sure the left padding on this.
    bytes.dropWhile(_.equals(0))
  }

  def bytes2bigint(a: Array[Byte]): BigInt = BigInt(1, a)

  /**
    * @return Formats unsigned byte as two hex characters, padding on left as needed (lowercase btw)
    */
  def ubyte2hex(v: UByte): String = zeroPadLeft(v.toHexString.toUpperCase, 2)

  def byte2ubyte(b: Byte): UByte = UByte(b)

  def ubytes2hex(v: Seq[UByte]): String = v.map(ubyte2hex).mkString

  /**
    * Takes an arbitrary length string and returns an listed of unsigned bytes
    * If the number of hex digits is odd, is padded with zero on left.
    */
  def hex2ubytes(v: String): Either[AppError, List[UByte]] = {
    zeroEvenPadHex(v).sliding(2, 2).toList.traverse(hex2ubyte)
  }

  /**
    *   Unsafe converstion of Hex to list of Unsigned Bytes.
    *   If hex is invalid then it throw Exception
    * @param v
    * @return
    */
  def unsafeHex2ubytes(v: String): List[UByte] = {
    hex2ubytes(v) match {
      case Right(list) ⇒ list
      case Left(err)   ⇒ throw new Exception(s"Programming Error $err")
    }
  }

  /**
    *   Note for speed
    * @param v Must be a one or two character hex string not enforced
    * @return
    */
  def hex2ubyte(v: String): Either[AppError, UByte] = {
    hex2byte(v).map(UByte(_))
  }

  def hex2byte(v:Hex): Either[AppError, Byte] = hex2byte(v.v)
  /**
    * Note for speed
    *
    * @param v Must be a one or two character hex string
    *
    * @return
    */
  def hex2byte(v: String): Either[AppError, Byte] = {
    AppException.wrap(s"$v hex to Byte") {
      java.lang.Long.parseLong(v, 16).toByte.asRight
    }
  }

  // FIXME: 32 bits instead of 8
  def ubyte2bitStr(v: UByte): String = zeroPadLeft(v.toInt.toBinaryString, 8)

  /** Binary formated with space between each byte */
  def ubytes2bitStr(lv: Seq[UByte]): String = {
    lv.map { v ⇒
        val str = v.toInt.toBinaryString
        zeroPadLeft(str, 8)
      }
      .mkString(" ")
  }

  def zeroPadLeft(v: String, len: Int): String = {
    val maxPad: String = "000000000000000000000000000000000000000000000000000000000000000000"
    len - v.length match {
      // Meh, this is Java 11 up, rarely hit this case so just recurse
      //case c if c > maxPad.length ⇒ "0".repeat(c) + v
      case c if c > maxPad.length ⇒ zeroPadLeft(maxPad+v, len)
      case c if c > 0             ⇒ maxPad.take(c) + v
      case c if c === 0           ⇒ v

    }
  }

  def zeroEvenPadHex(hex: String): String = {
    hex.length % 2 match {
      case 0 ⇒ hex
      case 1 ⇒ "0" + hex
    }
  }

  def trimLeftZeroBytes(a: Array[Byte]): Array[Byte] = {
    if (a.head != bytezero) a
    else trimLeftZeroBytes(a.tail)
  }

  def ulong2bitStr(v: ULong): String = {
    val str = v.toLong.toBinaryString
    zeroPadLeft(str, 64)
  }

  /** Quicky to take 16 hex chars and turn into ULong. Hex prefixed with 0x if missing */
  def hex2ulong(hex: String): Either[AppError, ULong] = {
    AppException.wrap(s"Parsing ULong from $hex") {
      val bi = BigInt(hex, 16)
      ULong.fromBigInt(bi).asRight
    }
  }

  /** If there are 8 bytes then return as ULong otherwise error. */
  def longBytesToULong(bytes: List[UByte]): Either[OError, ULong] = {

    if (bytes.length === 8) {
      // Convert to ULong 64

      val shifted: List[ULong] = bytes.mapWithIndex {
        case (b, indx) ⇒
          ULong(b.toLong) << ((7 - indx) * 8)
      }

      val ulong: ULong = shifted.foldLeft(ULong(0))(_ | _)
      ulong.asRight
    } else {
      AppError(s"8 Bytes needed to convert to ulong but ${bytes.length}").asLeft
    }
  }

  def ensureMaxLength(l: List[UByte], len: Int): Either[AppError, List[UByte]] = {
    if (l.length > len) AppError(s"Byte List length ${l.length} > $len").asLeft
    else l.asRight
  }

  def zeroPadBytes(l: List[UByte], len: Int): List[UByte] = {
    val padLen = len - l.length
    if (padLen > 0) {
      List.fill(padLen)(UByte(0)) ::: l
    } else {
      l
    }
  }

  def bytes2uint(bytes: Seq[Byte]): UInt = {
    val ints  = bytes.map(v ⇒ UInt(v.toLong))
    val shift = Seq(24, 16, 8, 0)

    (ints.head << 24) + (ints(1) << 16) + (ints(2) << 8) + ints(3)
  }

  def bytes2ulong(bytes: Seq[Byte]): UInt = {
    val ints = bytes.map(v ⇒ UInt(v.toLong))
    (ints.head << 24) + (ints(1) << 16) + (ints(2) << 8) + ints(3)
  }

  def uint2bytes(v: UInt): List[Byte] = {
    val mask     = UInt(255)
    val b4: UInt = mask & v
    val b3       = mask & (v >> 8)
    val b2       = mask & (v >> 16)
    val b1       = mask & (v >> 24)

    val longBytes: List[UInt] = List(b4, b3, b2, b1)
    //longBytes.forall(_.isValidByte) // Want valid unsigned Byte really
    val ans: List[Byte] = longBytes.map(v ⇒ v.signed.toByte)
    ans
  }

  def bytes2hex(bytes: Traversable[Byte]): String = {
    bytes.map(byte2hex).mkString
  }

  /** Returns String with exactly two uppercased Hex digits */
  def byte2hex(byte: Byte): String = {
    // byte.toHexString not happy
    val notPadded = UByte(byte).toHexString.toUpperCase
    zeroEvenPadHex(notPadded)
  }

}

object ByteUtils extends ByteUtils
