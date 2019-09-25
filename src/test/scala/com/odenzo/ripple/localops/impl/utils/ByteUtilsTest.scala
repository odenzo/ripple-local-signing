package com.odenzo.ripple.localops.impl.utils

import scala.collection.immutable

import cats._
import cats.data._
import cats.implicits._
import org.scalatest.FunSuite
import scribe.Level
import spire.math._

import com.odenzo.ripple.localops.LocalOpsError
import com.odenzo.ripple.localops.testkit.OTestSpec

class ByteUtilsTest extends OTestSpec with ByteUtils {

  import ByteUtils._

  test("Equals") {
    val mine: Byte     = 0.toByte
    val constant: Byte = ByteUtils.bytezero

    logger.debug(s"$mine and $constant  ${mine.hashCode()}  ${constant.hashCode()}")
    assert(mine == constant)
    assert(mine.eqv(constant))
  }

  test("UByte to Hex") {

    ByteUtils.ubyte2hex(UByte(1)) shouldEqual "01"
    ByteUtils.ubyte2hex(UByte(255)).equalsIgnoreCase("ff") shouldBe true

    val fullRange: String = (0 to 255)
      .map { n =>
        val hex = ByteUtils.ubyte2hex(UByte(n.toByte))
        s"$n  $hex"
      }
      .mkString("\n")
    logger.debug("Full Range to Hex\n" + fullRange)
  }

  test("UByte 2 Looping") {
    (0 to 255).foreach { n =>
      val byte: UByte = UByte(n.toByte)
      val hex: String = ByteUtils.ubyte2hex(byte)
      hex.length shouldEqual 2
      val back: UByte = getOrLog(ByteUtils.hex2ubyte(hex))

      logger.debug(s"$byte <-> $hex <-> $back")
      back shouldEqual byte

    }
  }

  test("HexToBytes") {
    val fixs: Seq[(String, UByte)] = (0 to 255).map { n =>
      val byte: UByte = UByte(n.toByte)
      val hex         = ubyte2hex(byte)

      (hex, byte)
    }
    val allHexDigits: String = fixs.map(_._1).mkString
    logger.debug(s"All Fex $allHexDigits")
    val calc: List[UByte] = getOrLog(hex2ubytes(allHexDigits))
    calc.length shouldBe 256
  }
}
