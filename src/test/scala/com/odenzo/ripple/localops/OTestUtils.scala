package com.odenzo.ripple.localops

import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import spire.math.{UByte, ULong}

import com.odenzo.ripple.localops.utils.ByteUtils
import com.odenzo.ripple.localops.utils.caterrors.AppError

trait OTestUtils extends StrictLogging {

  /**
    *
    * @param got  Value of field, without field marker
    * @param expected Expected value of fields without field marker
    * @return
    */
  def analyzeAmount(got: String, expected: String) = {

    val isXRP: Either[AppError, Boolean] = ByteUtils
      .hex2ubyte("0" + got.drop(2).head)
      .map(b ⇒ (b | UByte(8)) === UByte(0))

    isXRP.map {
      case true => logger.info("XRP Value Deal With IT")
      case false ⇒
        logger.info("Analyzing Suspected Fiat Amount")
        val gotFields: Either[AppError, (List[UByte], List[UByte], List[UByte])]      = breakFiat(got)
        val expectedFields: Either[AppError, (List[UByte], List[UByte], List[UByte])] = breakFiat(expected)
    }

  }

  /** Breaks down to UBytes for the amount, currency amd issuer */
  def breakFiat(hex: String): Either[AppError, (List[UByte], List[UByte], List[UByte])] = {

    val all: Either[AppError, List[UByte]] = ByteUtils.hex2ubytes(hex)
    val amount                             = all.map(_.take(8)) // Top 64 is amount in sign and flag
    val currency                           = all.map(_.slice(8, 28)) // 160 bits
    val issuer                             = all.map(_.slice(32, 52)) // another 160 bits
    (amount, currency, issuer).mapN((_, _, _))
  }

  /** Get Top 2 bits, Exponent (Shifted) anf the mantissa in that order in a list.
    * Moved down so the 2 bits in ULong has value 3 is both set etc.
    * */
  def breakFiatAmount(fields: ULong): List[ULong] = {

    // We char about the first 10 bits contains in the first two bytes

    val topMask: ULong      = ULong(0xC000000000000000L)
    val expMask: ULong      = ULong(0xFF) << 54
    val mantissaMask: ULong = ULong(0x3FFFFFFFFFFFFFL) // 13 nibbles

    //    logger.debug("Masks:\n" + ByteUtils.uLong2Base2Str(topMask)+
    //    "\n" + ByteUtils.uLong2Base2Str(expMask)+
    //    "\n" + ByteUtils.uLong2Base2Str(mantissaMask))

    val top2     = (fields & topMask) >> 62
    val exp      = (fields & expMask) >> 54
    val mantissa = fields & mantissaMask

    List(top2, exp, mantissa)
  }

}
