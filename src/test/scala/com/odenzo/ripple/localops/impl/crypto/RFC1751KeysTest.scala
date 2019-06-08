package com.odenzo.ripple.localops.impl.crypto

import org.scalatest.FunSuite

import com.odenzo.ripple.localops.impl.utils.ByteUtils
import com.odenzo.ripple.localops.testkit.OTestSpec

class RFC1751KeysTest extends FunSuite with OTestSpec {
  val words: String = "UTAH HONK SAT BLEW SHY RAP ELLA CHOU OSLO MAKE NUT RIG"
  val hex           = "6FD822DB25C72C7B6CBE93ACEA44D3F2"

  val fourwords: List[String] = words.split(' ').take(6).toList

  test("Scala") {
    getOrLog(RFC1751Keys.twelveWordsToHex(words)) shouldEqual hex
  }

  test("Scala 4") {
    val bytes = getOrLog(RFC1751Keys.etob(fourwords))
    val cHex  = ByteUtils.bytes2hex(bytes)
    cHex shouldEqual "F2D344EAAC93BE6C"
  }

  test("Ripple ED Wallet Ex") {
    val master_key             = "HICK LAUD TONY FORM SCOT DOES ORGY BUOY JUKE GLOB HUGE POE"
    val master_seed_hex        = "69C269468F0E4CC9E97C9DC2B667B597"
    val seed_bytes: List[Byte] = getOrLog(ByteUtils.hex2bytes(master_seed_hex))

    val b2e: String = getOrLog(RFC1751Keys.bytesToEnglish(seed_bytes.toArray))
    val e2b         = getOrLog(RFC1751Keys.twelveWordsToHex(master_key))

    b2e shouldEqual master_key
    e2b shouldEqual master_seed_hex
  }

}
