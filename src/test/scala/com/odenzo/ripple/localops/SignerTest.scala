package com.odenzo.ripple.localops

import spire.math.UByte

import com.odenzo.ripple.localops.impl.Signer
import com.odenzo.ripple.localops.impl.Signer.bytes2hex
import com.odenzo.ripple.localops.impl.crypto.core.HashOps
import com.odenzo.ripple.localops.impl.reference.HashPrefix
import com.odenzo.ripple.localops.impl.utils.ByteUtils
import com.odenzo.ripple.localops.testkit.OTestSpec

class SignerTest extends OTestSpec {

  test("Hash") {
    val txblob =
      "1200002280000000240000000161400000002114A0C0684000000002FAF0807321EDC5349AD8114DCDA07A355AA850FABE710CEE8FCBD891F1B919A6F6713C7BABA074401469964CFD9A724E69F1A64C313CD00567E9F8CFC7F330B59409749B4723E64C474BAE23D368E015616AC40CD0B1338F285617CFB0D6A434394C7DE4F4204F0E81142FF9D2D54B6D7E744EF5DEC5A27D3471D6AB690A83144194890464CAE2CB826CDBA5D938349184E385A0"

    val expHash = "1F51CBBAF7F23E6463BC4EF612D9995BDC13B965E2A6F0D3CE7D70C0DDD1F01B"
    makeHash(txblob) shouldEqual expHash
  }

  test("Web Data") {
    val txblob =
      "12001022800000002400000002201B0077911368400000000000000C694000000005F5E100732103B6FCD7FAC4F665FE92415DD6E8450AD90F7D6B3D45A6CFCF2E359045FF4BB400744630440220181FE2F945EBEE632966D5FB03114611E3047ACD155AA1BDB9DF8545C7A2431502201E873A4B0D177AB250AF790CE80621E16F141506CF507586038FC4A8E95887358114735FF88E5269C80CD7F7AF10530DAB840BBF6FDF8314A8B6B9FF3246856CADC4A0106198C066EA1F9C39"
    val expHash = "C0B27D20669BAB837B3CDF4B8148B988F17CE1EF8EDF48C806AE9BF69E16F441"

    val blob: List[UByte] = getOrLog(ByteUtils.hex2ubytes(txblob))
    logger.warn(
      s" txId Prefix: ${HashPrefix.transactionID.asBytes.toList.map(_.toChar)} " + HashPrefix.transactionID.asHex
    )
    val payload: List[UByte] = HashPrefix.transactionID.v ::: blob
    val hashBytes: Seq[Byte] = HashOps.sha512Half(payload.map(_.toByte))

    val hashHex = bytes2hex(hashBytes)
    logger.warn(s"HashHex: \n $hashHex \n $expHash ")
    hashHex shouldEqual expHash

    val okHex = Signer.createResponseHashHex(blob.map(_.toByte).toArray)
    logger.warn(s"OK HASH: $okHex")
    okHex shouldEqual hashHex

  }

  def makeHash(txBlob: String) = {

    // return new Hash256(sha512Half(HashPrefix.transactionID, serialized));
    val blob = getOrLog(ByteUtils.hex2bytes(txBlob))
    Signer.createResponseHashHex(blob.toArray)
//    val payload = HashPrefix.transactionID.asBytes ++ blob
//
//    val forward  = HashOps.sha512Half _ andThen HashOps.sha256
//    val backward = HashOps.sha256 _ andThen HashOps.sha512Half
//
//    val or = HashOps.sha256(HashPrefix.transactionID.asBytes ++ HashOps.sha512Half(blob))

    //bytes2hex(or)
  }
}
