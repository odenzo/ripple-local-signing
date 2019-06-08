package com.odenzo.ripple.localops.crypto.core

import org.scalatest.FunSuite

import com.odenzo.ripple.localops.OTestSpec

class HashingOpsTest extends FunSuite with OTestSpec with HashingOps {

  val text             = "Mary had a little lamb whose fleece was white as snow"
  val bytes: Seq[Byte] = text.getBytes("UTF-8").toSeq

  val otherBytes: Seq[Byte] = (text + text).getBytes("UTF-8").toSeq
  
  test("SHA512Half is SHA256") {
      sha512Half(bytes)  should not equal  sha256BC(bytes)
  }

  test("Diff Inputs") {

    val ops = Seq(sha256BC _, sha512Half _, sha512 _, sha512BC _,  sha256Ripple _, ripemd160 _)
    ops.foreach { op ⇒
      op(bytes) should not equal op(otherBytes)
    }
  }

}
