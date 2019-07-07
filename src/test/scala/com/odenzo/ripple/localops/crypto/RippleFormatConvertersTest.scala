package com.odenzo.ripple.localops.crypto

import org.scalatest.FunSuite

import com.odenzo.ripple.localops.testkit.OTestSpec

class RippleFormatConvertersTest extends OTestSpec {


  val jsonTxt =
    """
      |[
      |  {
      |    "request": {
      |      "command": "wallet_propose",
      |      "key_type": "secp256k1",
      |      "id": "87e1aae7-910f-46e2-88f8-e8ee36cd3d2b"
      |    },
      |    "response": {
      |      "id": "87e1aae7-910f-46e2-88f8-e8ee36cd3d2b",
      |      "result": {
      |        "account_id": "rBW1c6SEN2kJKX6KpZjRx6aXz1mhNRg4VA",
      |        "key_type": "secp256k1",
      |        "master_key": "SUMS SEW FOAM WINE HEN SOFA ME CURD GLUE THY ALEC OTIS",
      |        "master_seed": "shkdagkvurEgAqQodQU6nYQX8UHiH",
      |        "master_seed_hex": "93AB24A034BACD27C39F017E0E7607E5",
      |        "public_key": "aBP25NKvE9zt4ZTavmttWMZ5T4tMoMfG18mrT1DpZwWU8fDyAL6f",
      |        "public_key_hex": "02D5EACF650E008C25C43E5AF54C6B91792743F28F9BCB990ED6B8719A03391AEB"
      |      },
      |      "status": "success",
      |      "type": "response"
      |    }
      |  },
      | {
      |    "request": {
      |      "command": "wallet_propose",
      |      "key_type": "ed25519",
      |      "id": "7685e62f-0fd9-4661-bd54-2045675cbfb7"
      |    },
      |    "response": {
      |      "id": "7685e62f-0fd9-4661-bd54-2045675cbfb7",
      |      "result": {
      |        "account_id": "rayki11yG3rsF3TQqBYyszKEqyzFCuuHwM",
      |        "key_type": "ed25519",
      |        "master_key": "DUET CLAY WALE GALT LUST UN SALK NECK GOUT CALF MAW HOB",
      |        "master_seed": "ssTKNEB2aMu26PJW956jwoeECnMYX",
      |        "master_seed_hex": "367842313D5A18D6854E7BC4D9F3EC77",
      |        "public_key": "aKGGdRkrjx8FjpDzuudVuv8xffqcXErK2KGm2t6ac3Zq6scLz5gA",
      |        "public_key_hex": "EDC5F93A57986EC7A409D858210FA85B3238CFB48C843664C063B748BE768455F0"
      |      },
      |      "status": "success",
      |      "type": "response"
      |    }
      |  }
      |]
    """.stripMargin

  test("PublkeyKey2Address") {

  }
}
