package com.odenzo.ripple.localops.impl.crypto

import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.impl.crypto.core.ED25519CryptoBC
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.testkit.{FixtureUtils, OTestSpec}

class ED25519CryptoBCTest extends FunSuite with OTestSpec with FixtureUtils with JsonUtils with ByteUtils {

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
  private val txblob =
    "1200002280000000240000000961400000002114A0C0684000000002FAF0807321EDC5349AD8114DCDA07A355AA850FABE710CEE8FCBD891F1B919A6F6713C7BABA07440D67DE7CA3BECBBC941BD1ED5B8EAC77DC7E2FB8E6C64B58B570065E6C2B8B2323077131578A7D4E6F19B07B35E3E22E06CE1AA1DCE877F60F2EC324102F0E80381142FF9D2D54B6D7E744EF5DEC5A27D3471D6AB690A8314891A11D1ABD6C7010B29E60EF411F586690EC18E"
  private val txjson: String =
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

  val walletResult                       = JsonUtils.parseAsJson(wallet)
  val txjsonResult                       = JsonUtils.parseAsJson(txjson)
  val secretkey                          = "ANTE TUFT MEG CHEN CRAB DUMB COW OWNS ROOF FRED EDDY FORD"
  val seedB58                            = "spqnjaMMxPSvtaD4nevqqdjj4kzie"
  val seedHex                            = "09A117434757F90BF0BED6B29F185E4D"
  val signPubKey                         = "EDC5349AD8114DCDA07A355AA850FABE710CEE8FCBD891F1B919A6F6713C7BABA0"
  val sender                             = "rn4gsh2qp8842mTA5HfwGT3L1XepQCpqiu"
  val pubKeyHex                          = "C5349AD8114DCDA07A355AA850FABE710CEE8FCBD891F1B919A6F6713C7BABA0" // ED removed
  val kTxnSig                            = txjsonResult.flatMap(findFieldAsString("TxnSignature", _))
  val kHash                              = txjsonResult.flatMap(findFieldAsString("hash", _))
  val newKeyPar: AsymmetricCipherKeyPair = ED25519CryptoBC.generateKeyPair()

  test("BC") {
    import java.nio.charset.StandardCharsets
    import java.util.Base64

    import org.bouncycastle.crypto.params.{Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
    import org.bouncycastle.crypto.signers.Ed25519Signer
    // Test case defined in https://tools.ietf.org/html/rfc8037
    val msg         = "eyJhbGciOiJFZERTQSJ9.RXhhbXBsZSBvZiBFZDI1NTE5IHNpZ25pbmc".getBytes(StandardCharsets.UTF_8)
    val expectedSig = "hgyY0il_MGCjP0JzlnLWG1PPOt7-09PGcvMg3AIbQR6dWbhijcNR4ki4iylGjg5BhVsPt9g7sVvpAr_MuM0KAg"

    val privateKeyBytes = Base64.getUrlDecoder.decode("nWGxne_9WmC6hEr0kuwsxERJxWl7MmkZcDusAxyuf2A")
    val publicKeyBytes  = Base64.getUrlDecoder.decode("11qYAYKxCrfVS_7TyWQHOg7hcvPapiMlrwIaaPcHURo")

    // From B58
    // Expect Length 16
    // Prefix Expected:  var VER_K256: Array[Byte] = Array[Byte](B58IdentiferCodecs.VER_FAMILY_SEED.toByte)
    var VER_ED25519: Array[Byte] = Array[Byte](0x1.toByte, 0xe1.toByte, 0x4b.toByte)

    val privateKey = new Ed25519PrivateKeyParameters(privateKeyBytes, 0)
    val publicKey  = new Ed25519PublicKeyParameters(publicKeyBytes, 0)

    logger.info(s"PubHex: " + bytes2hex(publicKeyBytes))
    logger.info(s"PublicKey Encoded ${bytes2hex(publicKey.getEncoded)}")
    // Generate new signature
    val signer = new Ed25519Signer
    signer.init(true, privateKey)
    signer.update(msg, 0, msg.length)
    val signature       = signer.generateSignature
    val actualSignature = Base64.getUrlEncoder.encodeToString(signature).replace("=", "")

    logger.info(s"Expected signature: $expectedSig")
    logger.info(s"Actual signature  : $actualSignature")
  }
}
