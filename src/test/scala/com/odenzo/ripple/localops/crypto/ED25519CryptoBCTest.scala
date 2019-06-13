package com.odenzo.ripple.localops.crypto

import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.{AsymmetricKeyParameter, Ed25519PublicKeyParameters}
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.crypto.core.{ED25519CryptoBC, HashingOps}
import com.odenzo.ripple.localops.reference.HashPrefix
import com.odenzo.ripple.localops.utils.{ByteUtils, CirceUtils, FixtureUtils, JsonUtils}
import com.odenzo.ripple.localops.{OTestSpec, RippleLocalAPI}

class ED25519CryptoBCTest extends FunSuite with OTestSpec with FixtureUtils with JsonUtils with ByteUtils {

  val wallet =
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

  val txblob =
    "1200002280000000240000000961400000002114A0C0684000000002FAF0807321EDC5349AD8114DCDA07A355AA850FABE710CEE8FCBD891F1B919A6F6713C7BABA07440D67DE7CA3BECBBC941BD1ED5B8EAC77DC7E2FB8E6C64B58B570065E6C2B8B2323077131578A7D4E6F19B07B35E3E22E06CE1AA1DCE877F60F2EC324102F0E80381142FF9D2D54B6D7E744EF5DEC5A27D3471D6AB690A8314891A11D1ABD6C7010B29E60EF411F586690EC18E"

  val txjson =
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
                                  
  val walletResult = CirceUtils.parseAsJsonObject(wallet)
  val txjsonResult = CirceUtils.parseAsJsonObject(txjson)
  val secretkey    = "ANTE TUFT MEG CHEN CRAB DUMB COW OWNS ROOF FRED EDDY FORD"
  val seedB58      = "spqnjaMMxPSvtaD4nevqqdjj4kzie"
  val seedHex      = "09A117434757F90BF0BED6B29F185E4D"
  val signPubKey   = "EDC5349AD8114DCDA07A355AA850FABE710CEE8FCBD891F1B919A6F6713C7BABA0"
  val sender       = "rn4gsh2qp8842mTA5HfwGT3L1XepQCpqiu"
  val pubKeyHex    = "C5349AD8114DCDA07A355AA850FABE710CEE8FCBD891F1B919A6F6713C7BABA0" // ED removed

  val kTxnSig = txjsonResult.flatMap(findStringField("TxnSignature", _))
  val kHash   = txjsonResult.flatMap(findStringField("hash", _))

  val newKeyPar: AsymmetricCipherKeyPair = ED25519CryptoBC.nativeGenerateKeyPair()

  test("PubKey to Address") {
    val pub: List[Byte] = getOrLog(hex2Bytes(signPubKey))
    val account         = AccountFamily.accountpubkey2address(pub)
    logger.debug(s"Calculated Account/Account \n $account \n $sender")
    account shouldEqual sender
  }

  test("RFC Words") {
    val masterhex = AccountFamily.convertMasterKey2masterSeedHex(secretkey)
    logger.info(s"Secret Key $secretkey \nMaster Hex: $masterhex")
    masterhex shouldEqual seedHex
  }


  test("Private Key KeyPair") {
    val kp                               = getOrLog(ED25519CryptoBC.seedHex2keypair(seedHex))
    val akPublic: AsymmetricKeyParameter = kp.getPublic
    val edPublic                         = akPublic.asInstanceOf[Ed25519PublicKeyParameters]
    val pubEnc: Array[Byte]              = edPublic.getEncoded
    val pubHex                           = bytes2hex(pubEnc)
    logger.info(s"PubHex Generated from Seed $pubHex")
    pubHex shouldEqual signPubKey.drop(2)
  }

  test("Hash Computation") {

    // First lets see if we can get the hash
    for {
      tx_json    ← txjsonResult
      all        <- RippleLocalAPI.binarySerialize(tx_json)
      allHash    = HashingOps.sha512Half((HashPrefix.transactionID.v ::: all.rawBytes).map(_.toByte))
      allHashHex = ByteUtils.bytes2hex(allHash)
      _          = logger.info(s"All Fields ${all.fieldsInOrder}")
      _          = logger.info(s"AllHash ${ByteUtils.bytes2hex(allHash)}")
      _          = allHashHex shouldEqual kHash.right.value
    } yield all

  }

  test("Verification") {
    val ok = for {
      tx_json  ← txjsonResult
      keyPair  <- ED25519CryptoBC.seedHex2keypair(seedHex)
      pubHex   ← ED25519CryptoBC.publicKey2Hex(keyPair.getPublic)
      calcPub  ← ED25519CryptoBC.signingPubKey2KeyParameter(signPubKey)

      _        = pubHex shouldEqual signPubKey
      binBytes <- RippleLocalAPI.serializeForSigning(tx_json)
      toHash   = HashPrefix.transactionSig.asBytes ++ binBytes // Inner Transaction! 0x53545800L

      sig      ← kTxnSig
      sigBytes <- hex2Bytes(sig)
      pubKey  = keyPair.getPublic.asInstanceOf[Ed25519PublicKeyParameters]
      _ = calcPub.getEncoded shouldEqual pubKey.getEncoded
      verfied  = ED25519CryptoBC.edVerify(toHash, sigBytes.toArray,calcPub )
    } yield verfied
    val passed: Boolean = getOrLog(ok)
    passed shouldEqual true

  }

  test("Sign to TxnSignature") {
    val txnsig = for {
      tx_json  ← txjsonResult
      keyPair  <- ED25519CryptoBC.seedHex2keypair(seedHex)
      binBytes <- RippleLocalAPI.serializeForSigning(tx_json)
      toHash   = HashPrefix.transactionSig.asBytes ++ binBytes // Inner Transaction! 0x53545800L

      sigBytes = ED25519CryptoBC.edSign(toHash, keyPair)
      sigHex   = bytes2hex(sigBytes)
      _        = logger.info(s"EDSignature Len: ${sigBytes.length}")
    } yield sigHex
    getOrLog(txnsig)
    txnsig.right.value shouldEqual kTxnSig.right.value
  }

  test("Self txnscenarios And Verify") {
    // This produces repeatable signatures for given private key
    val keyPair = newKeyPar

    val sigVerified = for {
      tx_json ← txjsonResult

      binBytes <- RippleLocalAPI.serializeForSigning(tx_json)
      toHash   = HashPrefix.transactionSig.asBytes ++ binBytes // Inner Transaction! 0x53545800L
      hashed   = HashingOps.sha256Ripple(toHash.toSeq).toArray
      sigBytes = ED25519CryptoBC.edSign(hashed, keyPair)
      sign2    = ED25519CryptoBC.edSign(hashed, keyPair)
      sigHex   = bytes2hex(sigBytes)
      sig2Hex  = bytes2hex(sign2)
      _        = logger.info(s"Signs \n $sigHex \n $sig2Hex")
      _        = logger.info(s"Equal = ${sigHex === sig2Hex}")
      _        = logger.info(s"EDSignature Len: ${sigBytes.length}")
      pubKey   = keyPair.getPublic.asInstanceOf[Ed25519PublicKeyParameters]
      verfied = ED25519CryptoBC.edVerify(hashed, sigBytes, pubKey)
      _       = assert(verfied == true)
    } yield (sigHex, verfied)
    val (sig, ok) = getOrLog(sigVerified)
    logger.info(s"Verified; $ok")
    logger.info(s"Signature: $sig")
  }


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

    logger.info("Expected signature: {}", expectedSig)
    logger.info("Actual signature  : {}", actualSignature)
  }
}
