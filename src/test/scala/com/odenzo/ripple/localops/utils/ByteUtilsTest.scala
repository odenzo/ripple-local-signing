package com.odenzo.ripple.localops.utils

import scala.collection.immutable

import org.scalatest.FunSuite
import spire.math._

import com.odenzo.ripple.localops.testkit.OTestSpec
import com.odenzo.ripple.localops.utils.caterrors.AppError

class ByteUtilsTest extends FunSuite with OTestSpec {

  import ByteUtils._

  test("UByte to Hex") {

    ByteUtils.ubyte2hex(UByte(1)) shouldEqual "01"
    ByteUtils.ubyte2hex(UByte(255)).equalsIgnoreCase("ff") shouldBe true


    val fullRange: String = (0 to 255)
      .map { n ⇒
       val hex = ByteUtils.ubyte2hex(UByte(n.toByte))
        n.toString +" " + hex 
      }
      .mkString("\n")
    logger.debug("Full Range to Hex\n" + fullRange)
  }

  test("UByte 2 Looping") {
     (0 to 255). foreach{ n ⇒
        val byte: UByte = UByte(n.toByte)
        val hex: String = ByteUtils.ubyte2hex(byte)
        val back: Either[AppError, UByte] = ByteUtils.hex2ubyte(hex)

        logger.debug(s"$byte <-> $hex <-> $back")
        back.right.value shouldEqual byte

      }
  }



  test("UByte to Binary") {
    (0 to 255).foreach{ n ⇒
      val byte: UByte = UByte(n.toByte)
      val bin: String = ByteUtils.ubyte2bitStr(byte)

      logger.debug(s"$byte <-> $bin")
    }
  }

  test("ULong to Binary") {

    val full = ByteUtils.ulong2bitStr(ULong.MaxValue)
    logger.debug(s"Full: ${ULong.MaxValue} => $full")
    val empty = ByteUtils.ulong2bitStr(ULong.MinValue)
    logger.debug(s"Empty:${ULong.MinValue} => $empty")

  }

  test("HexToBytes") {
    val fixs: immutable.Seq[(String, UByte)] = (0 to 255).map{ n ⇒
       val byte: UByte = UByte(n.toByte)
       val hex = ubyte2hex(byte)
      hex.length shouldEqual 2
      (hex,byte)
    }
    val allHexDigits: String = fixs.map(_._1).mkString
    logger.debug(s"All Fex $allHexDigits")
    val calc: List[UByte] = hex2ubytes(allHexDigits).right.value
    calc.length shouldBe 256
  }
}
