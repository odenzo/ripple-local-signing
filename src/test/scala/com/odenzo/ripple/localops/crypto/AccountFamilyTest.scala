package com.odenzo.ripple.localops.crypto

import org.scalatest.FunSuite

import com.odenzo.ripple.localops._
import com.odenzo.ripple.localops.crypto.core.Secp256K1CryptoBC
import com.odenzo.ripple.localops.testkit.OTestSpec
import com.odenzo.ripple.localops.utils.ByteUtils

class AccountFamilyTest extends FunSuite with OTestSpec with ByteUtils with RippleFormatConverters {

  test("Genesis") { //masterpassphrase
    val json =
      """
        |{
        |	 "Request":{
        |    "command" : "wallet_propose",
        |    "seed" : null,
        |    "passphrase" : "masterpassphrase",
        |    "key_type" : "secp256k1",
        |    "id" : "6ebbdd79-8ea5-4b4e-aa2a-1464e323a219"
        |  },
        |	 "Response":{
        |    "id" : "6ebbdd79-8ea5-4b4e-aa2a-1464e323a219",
        |    "result" : {
        |        "account_id" : "rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh",
        |        "key_type" : "secp256k1",
        |        "master_key" : "I IRE BOND BOW TRIO LAID SEAT GOAL HEN IBIS IBIS DARE",
        |        "master_seed" : "snoPBrXtMeMyMHUVTgbuqAfg1SUTb",
        |        "master_seed_hex" : "DEDCE9CE67B451D852FD4E846FCDE31C",
        |        "public_key" : "aBQG8RQAzjs1eTKFEAQXr2gS4utcDiEC9wmi7pfUPTi27VCahwgw",
        |        "public_key_hex" : "0330E7FC9D56BB25D6893BA3F317AE5BCF33B3291BD63DB32654A313222F7FD020",
        |        "warning" : "This wallet was generated using a user-supplied passphrase that has low entropy and is vulnerable to brute-force attacks."
        |    },
        |    "status" : "success",
        |    "type" : "response"
        |  }
        |}
    """.stripMargin

  }

  test("Making a KeyPair") {
    // ANd also if I can just use a private key!
    val masterSeedHex = "559EDD35041D3C11F9BBCED912F4DE6A" // Ripple master seed is different than Private Key?

    val addr   = "r4kBGj4QWm1ord3fjd6KcGdENtzB4Fh4JS"
    val secret = "sh1aUhGupHaJG7gMeaxC6Nnde3Lnc"

    // Master seed is 32 bytes, need to find an ed255219 key from Ripple

  }

  test("Account Public Key to Address") {
    val pub                  = "0330E7FC9D56BB25D6893BA3F317AE5BCF33B3291BD63DB32654A313222F7FD020"
    val acct                 = "rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh"
    val sepBytes: List[Byte] = getOrLog(hex2bytes(pub))
    val csepAddr             = accountpubkey2address(sepBytes)

    csepAddr shouldEqual acct

    val edPubKey: String   = "ED9434799226374926EDA3B54B1B461B4ABF7237962EAE18528FEA67595397FA32"
    val edAddr: String     = "rDTXLQ7ZKZVKz33zJbHjgVShjsBnqMBhmN"
    val eBytes: List[Byte] = getOrLog(hex2bytes(edPubKey))

    val cedAddr = accountpubkey2address(eBytes)

    cedAddr shouldEqual edAddr
  }

  test("Wallet Shuffle") {
    val account_id      = "r49pwNZibgeK83BeEuHYFKBpJE5Tt4USsQ"
    val key_type        = "secp256k1"
    val master_key      = "TILE TAKE WELD CASK NEWT TIRE WIND SOFA SHED HELL TOOK FAR"
    val master_seed     = "ssDtFWc75geBLkzYcSYJ3nFbpRkaX"
    val master_seed_hex = "25DC4E4B6933FCFBD93F1CB2E6E3BCEB"
    val public_key      = "aBPHrChJfFe7MtwyPtpf82CsseoW2X22M8dS4eAjWdrWGBX48gk5"
    val public_key_hex  = "02ADBA6E42BCC1CEF0DA5CF2AC82A374C72ED7A78527976225D8AF49B82137934B"

    convertBase58Check2hex(master_seed).right.value shouldEqual master_seed_hex

    val accountKeys = getOrLog(AccountFamily.rebuildAccountKeyPairFromSeedHex(master_seed_hex))
    logger.info(s"Account KeyPair: ${accountKeys.getPublic}")
    val pubKeyBytesSmall: Array[Byte] = Secp256K1CryptoBC.compressPublicKey(accountKeys.getPublic)
    val compressedHex                 = bytes2hex(pubKeyBytesSmall)
    val compPubKey                    = Secp256K1CryptoBC.compressPublicKey(accountKeys.getPublic)
    val compPubKeyHex: String         = bytes2hex(compPubKey)
    compressedHex shouldEqual public_key_hex // Minus the 02 prefix in this case?
    compPubKeyHex shouldEqual public_key_hex
  }
}
