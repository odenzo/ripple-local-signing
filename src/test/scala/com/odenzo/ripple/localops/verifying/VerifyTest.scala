package com.odenzo.ripple.localops.verifying

import io.circe.JsonObject
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.impl.crypto.core.HashOps
import com.odenzo.ripple.localops.impl.reference.HashPrefix
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, CirceUtils, JsonUtils}
import com.odenzo.ripple.localops.testkit.{FixtureUtils, OTestSpec}
import com.odenzo.ripple.localops.{BinCodecProxy, RippleLocalAPI}

class VerifyTest extends FunSuite with OTestSpec with JsonUtils with FixtureUtils {

  test("Verify") {
    val tx_blob =
      "1200002280000000240000000161400000002114A0C0684000000003473BC073210375D8D111239BD845D7BC449D377043B0FE0D47998E035AE625E94482151B439D74463044022027C01E7079B7C55C099A164DA0458445A231E1F1982502F0C1AF3CD118D1114502204FFC17921BBC9647D2B07DA160D42ECF3844ECA0CF3115A8D96F5AC90A6CA4B58114AA8CB53A535F7B22D364FEA72195E3F2A30AF71B83141CA91D5A93D83BF6846059490E8F07F1EAB1953E"

    val txjson =
      """
        |{
        |      "Account" : "rGY8SwoHqdRrhZNiEwFBihaXpJ8hacrmoa",
        |      "Amount" : "555000000",
        |      "Destination" : "rscYWCWtgkhkTysdPpPfDfyAZR1B9yhuE6",
        |      "Fee" : "55000000",
        |      "Flags" : 2147483648,
        |      "Sequence" : 1,
        |      "SigningPubKey" : "0375D8D111239BD845D7BC449D377043B0FE0D47998E035AE625E94482151B439D",
        |      "TransactionType" : "Payment",
        |      "TxnSignature" : "3044022027C01E7079B7C55C099A164DA0458445A231E1F1982502F0C1AF3CD118D1114502204FFC17921BBC9647D2B07DA160D42ECF3844ECA0CF3115A8D96F5AC90A6CA4B5",
        |      "hash" : "AE3D99C014C518EF606B8FEB215C68913ABECF5D4CDE9FEB31044325EC6EE884"
        | }
    """.stripMargin

    val tx: JsonObject = getOrLog(CirceUtils.parseAsJson(txjson).flatMap(JsonUtils.json2object))
    val kHash          = findRequiredStringField("hash", tx)
    val kSigningPubKey = findRequiredStringField("SigningPubKey", tx)

    val hashOp: Seq[Byte] ⇒ Seq[Byte] = HashOps.sha512Half

    // First lets see if we can get the hash
    for {
      all <- BinCodecProxy.binarySerialize(tx)
      allHash    = hashOp((HashPrefix.transactionID.v ::: all.rawBytes).map(_.toByte))
      allHashHex = ByteUtils.bytes2hex(allHash)
      _          = logger.info(s"AllHash ${ByteUtils.bytes2hex(allHash)}")
      _          = allHashHex shouldEqual kHash
    } yield all

    val ok = RippleLocalAPI.verify(tx)
    logger.info(s"OK: $ok")
    ok.right.value shouldBe true

  }
}
