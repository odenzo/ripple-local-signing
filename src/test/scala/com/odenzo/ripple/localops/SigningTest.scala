package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import io.circe.{Json, JsonObject}
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.testkit.{AccountKeys, FixtureUtils, OTestSpec}
import com.odenzo.ripple.localops.utils.{ByteUtils, CirceUtils, JsonUtils}


class SigningTest extends FunSuite with OTestSpec with ByteUtils with FixtureUtils {

  val txnfixtures: List[(JsonObject, JsonObject)] = loadRequestResponses("/test/myTestData/keysAndTxn/secp256k1_txn.json")

  
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

  val keysJson: Json        = getOrLog(CirceUtils.parseAsJson(secp256k1_key), "Parsing Keys")
  val acctKeys: AccountKeys = getOrLog(CirceUtils.decode(keysJson, AccountKeys.decoder), "AccountKeys")
  val txjson: Json          = getOrLog(CirceUtils.parseAsJson(tx_json))

  // Need some complete messages
  test("Top Level txnscenarios") {

    logger.debug(s"Account Keys: $acctKeys")
    //val mainHex = getOrLog(BinarySerializerPublic.binarySerialize(json), "All")
    val jobj: JsonObject = getOrLog(JsonUtils.json2object(txjson), "TX_JSON not object")
    val tx_sig           = getOrLog(RippleLocalOps.serializeForSigning(jobj), "txnscenarios")
    logger.info(s"TXSIGNATURE: $tx_sig")

  }



}
