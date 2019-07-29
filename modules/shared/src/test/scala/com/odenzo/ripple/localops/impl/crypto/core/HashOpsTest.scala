package com.odenzo.ripple.localops.impl.crypto.core

import org.scalatest.FunSuite

import com.odenzo.ripple.localops.testkit.OTestSpec

class HashOpsTest extends FunSuite with OTestSpec with HashOps {

  val text             = "Mary had a little lamb whose fleece was white as snow"
  val bytes: Seq[Byte] = text.getBytes("UTF-8").toSeq

  val otherBytes: Seq[Byte] = (text + text).getBytes("UTF-8").toSeq

  test("SHA512Half is not SHA256") {
    sha512Half(bytes) should not equal sha256(bytes)
  }

  test("Test All Function No Exceptions") {

    sha256(bytes)
    sha512(bytes)
    sha512Half(bytes)
    ripemd160(bytes)
  }

}
