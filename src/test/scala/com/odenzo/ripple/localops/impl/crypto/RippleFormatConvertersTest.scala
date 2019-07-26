package com.odenzo.ripple.localops.impl.crypto

import cats._
import cats.data._
import cats.implicits._
import io.circe.{Decoder, Json}

import com.odenzo.ripple.localops.impl.utils.{ByteUtils, CirceUtils, Formatter, Hex}
import com.odenzo.ripple.localops.testkit.{AccountKeys, FixtureUtils, OTestSpec}

class RippleFormatConvertersTest extends OTestSpec with FixtureUtils with ByteUtils with RippleFormatConverters {

  val jsonTxt =
    """
      |[
      | {
      |        "account_id": "rBW1c6SEN2kJKX6KpZjRx6aXz1mhNRg4VA",
      |        "key_type": "secp256k1",
      |        "master_key": "SUMS SEW FOAM WINE HEN SOFA ME CURD GLUE THY ALEC OTIS",
      |        "master_seed": "shkdagkvurEgAqQodQU6nYQX8UHiH",
      |        "master_seed_hex": "93AB24A034BACD27C39F017E0E7607E5",
      |        "public_key": "aBP25NKvE9zt4ZTavmttWMZ5T4tMoMfG18mrT1DpZwWU8fDyAL6f",
      |        "public_key_hex": "02D5EACF650E008C25C43E5AF54C6B91792743F28F9BCB990ED6B8719A03391AEB"
      |      },
      | {
      |        "account_id": "rayki11yG3rsF3TQqBYyszKEqyzFCuuHwM",
      |        "key_type": "ed25519",
      |        "master_key": "DUET CLAY WALE GALT LUST UN SALK NECK GOUT CALF MAW HOB",
      |        "master_seed": "ssTKNEB2aMu26PJW956jwoeECnMYX",
      |        "master_seed_hex": "367842313D5A18D6854E7BC4D9F3EC77",
      |        "public_key": "aKGGdRkrjx8FjpDzuudVuv8xffqcXErK2KGm2t6ac3Zq6scLz5gA",
      |        "public_key_hex": "EDC5F93A57986EC7A409D858210FA85B3238CFB48C843664C063B748BE768455F0"
      |  }
      |]
    """.stripMargin

  test("PublkeyKey2Address") {

    val json: Json              = getOrLog(CirceUtils.parseAsJson(jsonTxt))
    val keys: List[AccountKeys] = getOrLog(CirceUtils.decode(json, Decoder[List[AccountKeys]]))

    keys.foreach(testKey)

    def testKey(k: AccountKeys) = {
      logger.info(s"AccountKeysL \n ${Formatter.prettyPrint(k)}")

      convertMasterKey2masterSeedHex(k.master_key.v.v).right.value shouldEqual k.master_seed_hex
      convertBase58Check2hex(k.master_seed.v.v).right.value shouldEqual k.master_seed_hex
      val seedHex = Hex(k.master_seed_hex)
      convertSeedHexToB58Check(seedHex).right.value.v shouldEqual k.master_seed.v.v

      convertBase58Check2hex(k.public_key.v.v).right.value shouldEqual k.public_key_hex
      convertPubKeyHexToB58Check(Hex(k.public_key_hex)).right.value.v shouldEqual k.public_key.v.v

      val pubKey = getOrLog(ByteUtils.hex2bytes(k.public_key_hex))
      accountpubkey2address(pubKey).right.value.v shouldEqual k.account_id.address.v

    }

  }

  test("Account Public Key to Address") {
    val pub                  = "0330E7FC9D56BB25D6893BA3F317AE5BCF33B3291BD63DB32654A313222F7FD020"
    val acct                 = "rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh"
    val sepBytes: List[Byte] = getOrLog(hex2bytes(pub))
    val csepAddr             = accountpubkey2address(sepBytes)

    csepAddr.right.value.v shouldEqual acct

    val edPubKey: String   = "ED9434799226374926EDA3B54B1B461B4ABF7237962EAE18528FEA67595397FA32"
    val edAddr: String     = "rDTXLQ7ZKZVKz33zJbHjgVShjsBnqMBhmN"
    val eBytes: List[Byte] = getOrLog(hex2bytes(edPubKey))

    val cedAddr = accountpubkey2address(eBytes)

    cedAddr.right.value.v shouldEqual edAddr
  }
}
