package com.odenzo.ripple.localops.crypto

import org.scalatest.FunSuite

import com.odenzo.ripple.localops.RFC1751
import scala.collection.JavaConverters._

import com.odenzo.ripple.localops.testkit.OTestSpec
import com.odenzo.ripple.localops.utils.ByteUtils

class RFC1751KeysTest extends FunSuite with OTestSpec {
  val words: String = "UTAH HONK SAT BLEW SHY RAP ELLA CHOU OSLO MAKE NUT RIG"
  val hex           = "6FD822DB25C72C7B6CBE93ACEA44D3F2"

  val fourwords: List[String] = words.split(' ').take(6).toList


  test("Scala") {
   getOrLog(RFC1751Keys.getKeyFromTwelveWords(words)) shouldEqual hex
  }



  test("Scala 4") {
    val bytes =  getOrLog(RFC1751Keys.etob(fourwords))
    val cHex = ByteUtils.bytes2hex(bytes)
    cHex  shouldEqual "F2D344EAAC93BE6C"
  }

}
