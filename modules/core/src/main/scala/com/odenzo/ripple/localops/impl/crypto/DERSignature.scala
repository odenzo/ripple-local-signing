package com.odenzo.ripple.localops.impl.crypto

import java.io.ByteArrayOutputStream
import java.math.BigInteger

import cats._
import cats.data._
import cats.implicits._
import org.bouncycastle.asn1.{ASN1Integer, DERSequenceGenerator}
import scribe.Logging

import com.odenzo.ripple.localops.LocalOpsError
import com.odenzo.ripple.localops.impl.crypto.core.Secp256K1CryptoBC
import com.odenzo.ripple.localops.impl.utils.ByteUtils

/** This is used just for DER encoding of ECDSA Signature for Secp256k1 Ripple stuff. */
case class DERSignature(thirty: String, zlen: String, r: DERField, s: DERField, ht: Option[String]) {

  def toByteList: Either[LocalOpsError, List[Byte]] = DERSignature.toByteList(this)
  def toBytes: Either[LocalOpsError, Array[Byte]]   = DERSignature.toByteList(this).map(_.toArray)
  def toHex: String                                 = thirty + zlen + r.toHex + s.toHex
}

case class DERField(twoPad: String, len: String, value: String) {
  val fieldLen: Int = value.length
  val totalLen: Int = twoPad.length + len.length + value.length

  val asBigInteger = new BigInteger(value, 16)

  /** We actually store in hex parts */
  def toHex: String = twoPad + len + value
}

object DERField extends Logging with ByteUtils {

  def toByteList(field: DERField): Either[LocalOpsError, List[Byte]] = {
    val parts: List[String]                    = List(field.twoPad, field.len, field.value)
    val ans: Either[LocalOpsError, List[Byte]] = parts.flatTraverse(p => hex2bytes(p))
    ans
  }

}

object DERSignature extends Logging with ByteUtils {

  def toByteList(sig: DERSignature): Either[LocalOpsError, List[Byte]] = {
    for {
      t <- hex2bytes(sig.thirty)
      z <- hex2bytes(sig.zlen)
      r <- DERField.toByteList(sig.r)
      s <- DERField.toByteList(sig.s)
    } yield List(t, z, r, s).flatten
  }

  def parseDERField(hex: String): Either[LocalOpsError, DERField] = {
    val twoPad: String                     = hex.take(2)
    val lenHex: String                     = hex.slice(2, 4)
    val lenInt: Either[LocalOpsError, Int] = ByteUtils.hex2ulong(lenHex).map(_.toInt * 2) // Hex

    lenInt.map { len =>
      val value = hex.slice(2 + 2, 2 + 2 + len)
      DERField(twoPad, lenHex, value)
    }

  }

  /** This should work staight on TxnSignature field */
  def fromHex(hex: String): Either[LocalOpsError, DERSignature] = hex2bytes(hex).flatMap(fromBytes)

  /** To check if in "standard" format
    *
    * @return list of fields in hex format */
  def fromBytes(sig: List[Byte]): Either[LocalOpsError, DERSignature] = {

    val hexErr: Either[Nothing, String] = ByteUtils.bytes2hex(sig.toArray).asRight

    // I want to use a consumable stream... where take gets and removes

    val fields: Either[LocalOpsError, (DERSignature, String)] = for {
      hex <- hexErr
      _      = logger.debug(s"Hex Len= ${hex.length} :\n $hex")
      thirty = hex.take(2)
      zlen   = hex.slice(2, 4)
      r <- parseDERField(hex.drop(4))
      s <- parseDERField(hex.drop(4 + r.totalLen))
      rsLen = r.totalLen + s.totalLen
      ht    = hex.slice(4 + rsLen, 4 + rsLen + 2) // Ripple seems to add the ht to 'S' len ?
      over  = hex.drop(4 + rsLen + 2)

      sig = DERSignature(thirty, zlen, r, s, Some(ht))

    } yield (sig, over)

    fields.flatMap {
      case (sig, over) if (over.isEmpty) => sig.asRight
      case (sig, over)                   => LocalOpsError(s"Overflow on DERSig $over").asLeft
    }
  }

  def fromRandS(r: BigInteger, s: BigInteger): Either[LocalOpsError, DERSignature] = {
    // https://bitcoin.stackexchange.com/questions/2376/ecdsa-r-s-encoding-as-a-signature
    // Try and use a lib
    logger.debug(s"r = $r   s = $s")

    // JCA does it, but sometimes we need to pack zero in front of r and s
    // It appears zlen is calculated not including ht
    // And S has an extra zero byte in from
    // I think we may just want to trim to get them to size?

    val rBytes: Array[Byte] = ByteUtils.bigint2bytes(r)
    val rLen: Array[Byte]   = Array(rBytes.length.toByte)
    val sBytes: Array[Byte] = ByteUtils.bigint2bytes(s)
    val sLen: Array[Byte]   = Array(sBytes.length.toByte)

    // ht is hashtype, really, WTF Forgot where this came from It is not in TxnSignature
    // I think this is a bitcoin only thing.
    val ans: Either[LocalOpsError, List[Byte]] = for {
      thirty <- ByteUtils.hex2bytes("30")
      two    <- ByteUtils.hex2bytes("02")
      ht     <- ByteUtils.hex2bytes("6D")
      backPart = two ++ rLen ++ rBytes ++ two ++ sLen ++ sBytes
      zLen     = List(backPart.length.toByte)
      all      = thirty ++ zLen ++ backPart
      _        = logger.debug(s"$thirty  $two $rLen $r $two $sLen $s $ht")

    } yield all
    ans.flatMap(DERSignature.fromBytes)

  }

  /** s should be less then CurveOrder - s, this returns true if it is else false
    * using secp256k1 curve */
  def checkIfSignatureCanonical(r: BigInteger, s: BigInteger): Boolean = {
    val order: BigInteger = Secp256K1CryptoBC.params.getN
    val invS: BigInteger  = order.subtract(s)
    // S should be less than invS
    (s.compareTo(invS) <= 0) // S > invS  then false

  }

  protected def derBytes(r: BigInteger, s: BigInteger): Array[Byte] = { // Usually 70-72 bytes.
    val bos = new ByteArrayOutputStream(72)
    val seq = new DERSequenceGenerator(bos)
    seq.addObject(new ASN1Integer(r))
    seq.addObject(new ASN1Integer(s))
    seq.close()
    bos.toByteArray
  }

}
