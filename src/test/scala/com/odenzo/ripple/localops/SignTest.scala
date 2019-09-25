package com.odenzo.ripple.localops

import io.circe.Json

import spire.math.UByte

import com.odenzo.ripple.localops.impl.Sign.bytes2hex
import com.odenzo.ripple.localops.impl.crypto.core.HashOps
import com.odenzo.ripple.localops.impl.reference.HashPrefix
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.impl.{BinCodecProxy, Sign}
import com.odenzo.ripple.localops.testkit.{AccountKeys, OTestSpec}

class SignTest extends OTestSpec {

  // An inactivated account
  val secp256k1_key = """ {
                        |    "account_id": "rDGnaDqJczDAjrKHKdhGRJh2G7zJfZhj5q",
                        |    "key_type": "secp256k1",
                        |    "master_key": "COON WARN AWE LUCK TILE WIRE ELI SNUG TO COVE SHAM NAT",
                        |    "master_seed": "sstV9YX8k7yTRzxkRFAHmX7EVqMfX",
                        |    "master_seed_hex": "559EDD35041D3C11F9BBCED912F4DE6A",
                        |    "public_key": "aBQXEw1vZD3guCX3rHL8qy8ooDomdFuxZcWrbRZKZjdDkUoUjGVS",
                        |    "public_key_hex": "0351BDFB30E7924993C625687AE6127034C4A5EBA78A01E9C58B0C46E04E3A4948"
                        |  }""".stripMargin

  val tx_json =
    """ {
      | "Account": "r9LqNeG6qHxjeUocjvVki2XR35weJ9mZgQ"   ,
      | "Amount": "1000",
      | "Destination": "rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh",
      | "Fee": "10",
      | "Flags": 2147483648,
      | "Sequence": 1,
      | "TransactionType": "Payment"
      |  }
  """.stripMargin

  val sign_result =
    """ {
      | "Account": "r9LqNeG6qHxjeUocjvVki2XR35weJ9mZgQ"    ,
      | "Amount": "1000",
      | "Destination": "rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh",
      | "Fee": "10",
      | "Flags": 2147483648,
      | "Sequence": 1,
      | "TransactionType": "Payment",
      | "TxnSignature":
      | "30440220718D264EF05CAED7C781FF6DE298DCAC68D002562C9BF3A07C1E721B420C0DAB02203A5A4779EF4D2CCC7BC3EF886676D803A9981B928D3B8ACA483B80ECA3CD7B9B",
      | "Signature":
      | "30440220718D264EF05CAED7C781FF6DE298DCAC68D002562C9BF3A07C1E721B420C0DAB02203A5A4779EF4D2CCC7BC3EF886676D803A9981B928D3B8ACA483B80ECA3CD7B9B",
      | "SigningPubKey": "ED5F5AC8B98974A3CA843326D9B88CEBD0560177B973EE0B149F782CFAA06DC66A"
      |
  }
  """.stripMargin

  val keysJson: Json        = getOrLog(JsonUtils.parseAsJson(secp256k1_key), "Parsing Keys")
  val acctKeys: AccountKeys = getOrLog(JsonUtils.decode(keysJson, AccountKeys.decoder), "AccountKeys")
  val txjson: Json          = getOrLog(JsonUtils.parseAsJson(tx_json))

  // Need some complete messages
  test("Top Level txnscenarios") {

    logger.debug(s"Account Keys: $acctKeys")
    //val mainHex = getOrLog(BinarySerializerPublic.binarySerialize(json), "All")
    val tx_sig = getOrLog(BinCodecProxy.serializeForSigning(txjson), "txnscenarios")
    logger.info(s"TXSIGNATURE: $tx_sig")

  }

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

    val blob: List[UByte]    = getOrLog(ByteUtils.hex2ubytes(txblob))
    val payload: List[UByte] = HashPrefix.transactionID.v ::: blob
    val hashBytes: Seq[Byte] = HashOps.sha512Half(payload.map(_.toByte).toArray)

    val hashHex = bytes2hex(hashBytes)
    logger.debug(s"HashHex: \n $hashHex \n $expHash ")
    hashHex shouldEqual expHash

    val okHex = Sign.createResponseHashHex(blob.map(_.toByte).toArray)
    logger.debug(s"OK HASH: $okHex")
    okHex shouldEqual hashHex

  }

  def makeHash(txBlob: String): String = {

    // return new Hash256(sha512Half(HashPrefix.transactionID, serialized));
    val blob = getOrLog(ByteUtils.hex2bytes(txBlob))
    Sign.createResponseHashHex(blob.toArray)
//    val payload = HashPrefix.transactionID.asBytes ++ blob
//
//    val forward  = HashOps.sha512Half _ andThen HashOps.sha256
//    val backward = HashOps.sha256 _ andThen HashOps.sha512Half
//
//    val or = HashOps.sha256(HashPrefix.transactionID.asBytes ++ HashOps.sha512Half(blob))

    //bytes2hex(or)
  }
}
