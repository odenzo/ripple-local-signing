package com.odenzo.ripple.localops.crypto

import io.circe.Json
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.utils.CirceUtils
import com.odenzo.ripple.localops.{AccountKeys, OTestSpec}

class CryptoOpsTest extends FunSuite with OTestSpec {

  val secp256k1_key =
    """ {
      |    "account_id": "rDGnaDqJczDAjrKHKdhGRJh2G7zJfZhj5q",
      |    "key_type": "secp256k1",
      |    "master_key": "COON WARN AWE LUCK TILE WIRE ELI SNUG TO COVE SHAM NAT",
      |    "master_seed": "sstV9YX8k7yTRzxkRFAHmX7EVqMfX",
      |    "master_seed_hex": "559EDD35041D3C11F9BBCED912F4DE6A",
      |    "public_key": "aBQXEw1vZD3guCX3rHL8qy8ooDomdFuxZcWrbRZKZjdDkUoUjGVS",
      |    "public_key_hex": "0351BDFB30E7924993C625687AE6127034C4A5EBA78A01E9C58B0C46E04E3A4948"
      |  }""".stripMargin

  // Not sure how to keygen this kind of key or what it is called. edsa can keygen
  val keysJson: Json        = getOrLog(CirceUtils.parseAsJson(secp256k1_key), "Parsing Keys")
  val acctKeys: AccountKeys = getOrLog(CirceUtils.decode(keysJson, AccountKeys.decoder), "AccountKeys")

  val blank: Seq[Byte] = List.fill[Byte](32)(0)



  test("edsa") {
//    val edKey: KeyPair      = JavaCryptoClient.genKeyPair(JavaCryptoClient.ed25519)
//    val signed: Array[Byte] = JavaCryptoClient.signEd25519(blank.toArray, edKey.getPrivate.getEncoded)
//    val hex = ByteUtils.ubytes2Hex(signed.map(UByte.apply).toSeq)
//    logger.debug(s"Hex ed: ${signed.length} " + hex)
  }

  
}
