package com.odenzo.ripple.localops.impl.crypto.core

import com.odenzo.ripple.localops.impl.utils.ByteUtils
import com.odenzo.ripple.localops.testkit.OTestSpec

class HashOpsTest extends OTestSpec with HashOps with ByteUtils {

  val hashFns: List[Array[Byte] => IndexedSeq[Byte]] = List(sha256, sha512, sha512Half, ripemd160)
  val text                                           = "Mary had a little lamb whose fleece was white as snow"
  val bytes                                          = text.getBytes("UTF-8")
  val otherBytes                                     = (text + text).getBytes("UTF-8")
  val emptyPayload                                   = Seq.empty[Byte]

  test("SHA512Half is not SHA256") {
    sha512Half(bytes) should not equal sha256(bytes)
  }

  test("Test All Function No Exceptions") {

    hashFns.foreach { fn =>
      val good  = fn(bytes)
      val other = fn(otherBytes)
      (good === other) shouldBe false

    }

  }

  test("Empty Bytes") {
    hashFns.foreach { fn =>
      val good = fn(emptyPayload.toArray)
    }
  }

  test("Web Values") {
    val sha512Hash =
      "6EAC10E1D1B1A46ED84C203CC12991E4D8B4884E96A8B12966564640F7B739FD2CB441185A3AC91D590AE2A3AC12F769A4F217C4F38668B8128AD3E30A513946"
    val sha256Hash = "A9A3EDA7AD0D38340CC3A74544F83235F20220DB856C65885E0E11ACB136E5B2"
    val ripemdHash = "2C0C44C702781FA691B6E2EA715EE9DC4D9EE5AE"

    bytes2hex(sha512(bytes)) shouldEqual sha512Hash
    bytes2hex(sha256(bytes)) shouldEqual sha256Hash
    bytes2hex(ripemd160(bytes)) shouldEqual ripemdHash
  }
}
