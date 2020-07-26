package com.odenzo.ripple.localops

import io.circe.{Decoder, Json}

import scribe.Level

import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.impl.{BinCodecProxy, Sign}
import com.odenzo.ripple.localops.models.TxnSignature
import com.odenzo.ripple.localops.testkit.OTestSpec

/**
  * With fixed data showing the AccountKeys, the txBlob and a tx_json we should be able to incrementally
  * shake down:
  * 1. Sanity test on binarySerialize and binarySerializeForSigning
  * 2. Check TxnSignature correctly calculatutes
  * 3. Check TxBlob correctly calculated
  * 4. Check the hash is calculated correctly.
  */
class ShakeDownTest extends OTestSpec with ByteUtils {

  private val wallet =
    """
    {
      |        "account_id": "rn4gsh2qp8842mTA5HfwGT3L1XepQCpqiu",
      |        "key_type": "ed25519",
      |        "master_key": "ANTE TUFT MEG CHEN CRAB DUMB COW OWNS ROOF FRED EDDY FORD",
      |        "master_seed": "spqnjaMMxPSvtaD4nevqqdjj4kzie",
      |        "master_seed_hex": "09A117434757F90BF0BED6B29F185E4D",
      |        "public_key": "aKGGHoqb2C2Xj6qtzikTTdsQdPcnYS8ue4XzXvT2T6fuofFP4zrA",
      |        "public_key_hex": "EDC5349AD8114DCDA07A355AA850FABE710CEE8FCBD891F1B919A6F6713C7BABA0"
      |      }
    """.stripMargin
  private val kTxBlob =
    "1200002280000000240000000961400000002114A0C0684000000002FAF0807321EDC5349AD8114DCDA07A355AA850FABE710CEE8FCBD891F1B919A6F6713C7BABA07440D67DE7CA3BECBBC941BD1ED5B8EAC77DC7E2FB8E6C64B58B570065E6C2B8B2323077131578A7D4E6F19B07B35E3E22E06CE1AA1DCE877F60F2EC324102F0E80381142FF9D2D54B6D7E744EF5DEC5A27D3471D6AB690A8314891A11D1ABD6C7010B29E60EF411F586690EC18E"
  private val txjsonStr: String =
    """
      | {
      |          "Account": "rn4gsh2qp8842mTA5HfwGT3L1XepQCpqiu",
      |          "Amount": "555000000",
      |          "Destination": "rDVvod7r7saJPgLDZHazu11oURPa2xCrP3",
      |          "Fee": "50000000",
      |          "Flags": 2147483648,
      |          "Sequence": 9,
      |          "SigningPubKey": "EDC5349AD8114DCDA07A355AA850FABE710CEE8FCBD891F1B919A6F6713C7BABA0",
      |          "TransactionType": "Payment",
      |          "TxnSignature": "D67DE7CA3BECBBC941BD1ED5B8EAC77DC7E2FB8E6C64B58B570065E6C2B8B2323077131578A7D4E6F19B07B35E3E22E06CE1AA1DCE877F60F2EC324102F0E803",
      |          "hash": "2EFB6703751A981BEBF0274088C13FF01BEBEEDE7A758274715839ADB5645510"
      | }
      |
    """.stripMargin

  val walletResult: Json = getOrLog(JsonUtils.parseAsJson(wallet))
  val txjson             = getOrLog(JsonUtils.parseAsJson(txjsonStr))
  val walletMap          = decode(walletResult, Decoder[Map[String, String]])
  val kTxnSig: String    = getOrLog(findFieldAsString("TxnSignature", txjson))
  val kHash              = getOrLog(findFieldAsString("hash", txjson))

  test("Low Level") {
    setTestLogLevel(Level.Debug)
    val complete = for {
      signing  <- BinCodecProxy.binarySerializeForSigning(txjson)
      blobbing <- BinCodecProxy.binarySerialize(txjson)

      txSigField = TxnSignature(kTxnSig)
      txblob <- Sign.createSignedTxBlob(txjson, txSigField)
      txblobhex = ByteUtils.bytes2hex(txblob)
      hash      = Sign.createResponseHashHex(blobbing.toBytes)
      _         = logger.debug(s"Hash = $hash")
      _         = logger.debug(s"TxBlob No Crypto: ${txblobhex}")
      _         = kHash shouldEqual hash
      _         = txblobhex shouldEqual kTxBlob

    } yield hash
    getOrLog(complete)
  }

  test("Recreate TxnSig") {
    setTestLogLevel(Level.Debug)
    val complete = for {
      signkey <- RippleLocalAPI.packSigningKey("09A117434757F90BF0BED6B29F185E4D", "ed25519")
      txnsig  <- RippleLocalAPI.signToTxnSignature(txjson, signkey)
      _ = logger.debug(s"TxnSIg: $txnsig")
      _ = txnsig shouldEqual kTxnSig
    } yield txnsig
    getOrLog(complete)
  }

  test("Verify") {
    setTestLogLevel(Level.Debug)
    val complete = for {
      verified <- RippleLocalAPI.verify(txjson)
      _ = logger.debug(s"TxnSIg: $verified")
      _ = verified shouldEqual true
    } yield verified
    getOrLog(complete)
  }
}
