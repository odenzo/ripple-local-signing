package com.odenzo.ripple.localops

import java.util.UUID

import cats._
import cats.data._
import cats.implicits._

import io.circe.syntax._
import io.circe.{Decoder, Json, JsonObject}

import com.odenzo.ripple.localops.impl.SignFor
import com.odenzo.ripple.localops.impl.messagehandlers.SignForMsg
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, CirceUtils}
import com.odenzo.ripple.localops.testkit.{FKP, MultiSignTrace, OTestSpec, Signer, Signers}

class SignForTest extends OTestSpec with ByteUtils with SignFor {

  private val tracer =
    """
      |  {
      |    "account" : {
      |        "account_id" : "rfPzbUC8ckh5WoAxThpuCrjHbNP8D7HNkf",
      |        "key_type" : "secp256k1",
      |        "master_key" : "BASE OLDY LUG ROSS COLD GIL LION FOR NO DUAL DEE MIRE",
      |        "master_seed" : "sh4xb525MafwxrW47YEE72kFWRwVr",
      |        "master_seed_hex" : "78DBB03BB19842B02E9E466999F85853",
      |        "public_key" : "aB4VtezsoJB9eAf6cbf79FGM2L7PeLxKXXVF7otknLUWbAV81hR9",
      |        "public_key_hex" : "024746B1C2A88A59B4A1F40F8F11042D4090DA2AB55775A61EF03184809DE3E7CD"
      |    },
      |    "signers" : [
      |        {
      |            "account_id" : "r9EP7xcWBAWEHhgtm4evqsHTJT4ZesJHXX",
      |            "key_type" : "secp256k1",
      |            "master_key" : "DUCK BOAR SWIM HAYS TORN ADDS WEAL HURL MET PEA HUGO FOUL",
      |            "master_seed" : "spiFT8g6WPscjnZVxBWKeZmX5155Z",
      |            "master_seed_hex" : "0BC75919A1A4F3F891E49ECA99478B77",
      |            "public_key" : "aBP3fmQVYDDTrDVv7DTeRg7ZqwdV4gvaMYyz4S6U8kiLToSFEzUn",
      |            "public_key_hex" : "0299882568B5CFE3620AA1AD7EA19F76222F7B92AA6A75C80CF28E3746358030A4"
      |        },
      |        {
      |            "account_id" : "rK6RuzCeWzqz19jeaHd89uZjgJbQfJnBYG",
      |            "key_type" : "secp256k1",
      |            "master_key" : "AGEE LONG AMES BET NEST ARID USED BITE ROWS GOAL CHUB THIS",
      |            "master_seed" : "snCjAj5HcveSk1287bjMaWxmvbKXo",
      |            "master_seed_hex" : "D567D6464D136BF29C3A8C0330417649",
      |            "public_key" : "aBQF8BUGCPAHN2rP9dTKya1WPuWZPnTXmBJKnL5wm2VdxCRVWZrk",
      |            "public_key_hex" : "03798E77D176DFE07647A4C77983747396CEE214CEDC14699425D993FBA03F8FD0"
      |        },
      |        {
      |            "account_id" : "rnSkJvw3eBZzLbZctN4d6tACeMmB5zPP9b",
      |            "key_type" : "secp256k1",
      |            "master_key" : "ARMY SLAY TEE SKIM DANE TOY OMIT RIDE GAIN AIDS LAMB RUN",
      |            "master_seed" : "shJ5yAncYteQBgxAUC2LbAVqUSNXh",
      |            "master_seed_hex" : "739E0A2521EE79C884EEE6EEFFD05B4E",
      |            "public_key" : "aBQt1Ak51NWZqLfNnjwJDhkNNKNq1GT28qPwB7AMHyZ4pegRvass",
      |            "public_key_hex" : "0382DD652CD83FF367A3B7E63493C4F82BD04803DF7DC036244122F51F2DBD94D6"
      |        },
      |        {
      |            "account_id" : "rUAAGmayXTWYdm8tccWJuaN2HhZYSmsp5C",
      |            "key_type" : "secp256k1",
      |            "master_key" : "MUCK CASK LIST TACT BOSS NELL HERO MUCH SAL HAIR DONE DADE",
      |            "master_seed" : "snmLhWjtCCm8R9Ebx5gZtYoibwpqG",
      |            "master_seed_hex" : "DC5247C9E8F8379786D96573C286ECBF",
      |            "public_key" : "aBR2xTTzhUY8sNnDc2C2o5KnYj1hh583dWTaYLwzfWshFizYZc4E",
      |            "public_key_hex" : "03DDDDCA4366C40420819409039DE0326F5E2109F1521143CA1FFF39D69B5FFD7C"
      |        }
      |    ],
      |    "signFors" : [
      |        {
      |            "rq" : {
      |                "command" : "sign_for",
      |                "account" : "r9EP7xcWBAWEHhgtm4evqsHTJT4ZesJHXX",
      |                "tx_json" : {
      |                    "TransactionType" : "Payment",
      |                    "Account" : "rfPzbUC8ckh5WoAxThpuCrjHbNP8D7HNkf",
      |                    "Amount" : "10000000",
      |                    "Destination" : "rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh",
      |                    "Sequence" : 2,
      |                    "Fee" : "50000000",
      |                    "Flags" : 2147483648,
      |                    "SigningPubKey" : ""
      |                },
      |                "seed_hex" : "0BC75919A1A4F3F891E49ECA99478B77",
      |                "key_type" : "secp256k1",
      |                "id" : "f3df1965-0d65-4f0e-913d-8da469768e5e"
      |            },
      |            "rs" : {
      |                "id" : "f3df1965-0d65-4f0e-913d-8da469768e5e",
      |                "result" : {
      |                    "deprecated" : "This command has been deprecated and will be removed in a future version of the server. Please migrate to a standalone signing tool.",
      |                    "tx_blob" : "12000022800000002400000002614000000000989680684000000002FAF080730081144629FAB38D9697CCA3BF48F472E1145630D95B778314B5F762798A53D543A014CAF8B297CFF8F2F937E8F3E01073210299882568B5CFE3620AA1AD7EA19F76222F7B92AA6A75C80CF28E3746358030A47446304402207ADF9E5014A2E0589F05394EA87DFDEDD88E045398E537EB0236D87E117ED9EB022065EA520F29EB39C5A3DC84091D1FE89800FEF884DDAA00F832F2E4EC7BAD592D81145A7998C82DF2A2D0EEC2DCEB040DD7C291E408EAE1F1",
      |                    "tx_json" : {
      |                        "Account" : "rfPzbUC8ckh5WoAxThpuCrjHbNP8D7HNkf",
      |                        "Amount" : "10000000",
      |                        "Destination" : "rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh",
      |                        "Fee" : "50000000",
      |                        "Flags" : 2147483648,
      |                        "Sequence" : 2,
      |                        "Signers" : [
      |                            {
      |                                "Signer" : {
      |                                    "Account" : "r9EP7xcWBAWEHhgtm4evqsHTJT4ZesJHXX",
      |                                    "SigningPubKey" : "0299882568B5CFE3620AA1AD7EA19F76222F7B92AA6A75C80CF28E3746358030A4",
      |                                    "TxnSignature" : "304402207ADF9E5014A2E0589F05394EA87DFDEDD88E045398E537EB0236D87E117ED9EB022065EA520F29EB39C5A3DC84091D1FE89800FEF884DDAA00F832F2E4EC7BAD592D"
      |                                }
      |                            }
      |                        ],
      |                        "SigningPubKey" : "",
      |                        "TransactionType" : "Payment",
      |                        "hash" : "94ABE767BBECED4D02824B94ECF993A771714CCCB037A9F3C32C45C937E922D7"
      |                    }
      |                },
      |                "status" : "success",
      |                "type" : "response"
      |            }
      |        }
      |    ]
      |}
      |""".stripMargin

  /** Single Signing of tx_json returning the entire result response */
  def signFor(tx_json: JsonObject, fkp: FKP): JsonObject = {

    val rq = JsonObject(
      "id"       := UUID.randomUUID(),
      "command"  := "sign_for",
      "account"  := fkp.master.address,
      "seed_hex" := fkp.master.master_seed_hex,
      "key_type" := fkp.master.key_type,
      "tx_json"  := tx_json
    )

    val rqTrimmed = droppingNullsSortedPrinter.pretty(rq.asJson)

    logger.info("Request Object (First): " + rqTrimmed)
    val rs             = SignForMsg.signFor(rq.asJson.dropNullValues)
    val ok: JsonObject = rs.fold(a => a, b => b) // Shortcut Left or Right?
    ok
  }

  //

  /** Incremental Dev Tests */
  test("signFor TDP") {
    //OTestLogging.setTestLogLevel(Level.Debug)
    val trace: MultiSignTrace = getOrLog(CirceUtils.parseAndDecode(tracer, MultiSignTrace.decoder))

    val firstRq        = trace.signFors.head.rq
    val firstRs        = trace.signFors.head.rs
    val rs             = SignForMsg.signFor(firstRq.asJson)
    val ok: JsonObject = rs.fold(a => a, b => b)

    checkResults(ok, firstRs)

    logger.info(s"Result ${ok.asJson.spaces4}")
    // TODO: Complete this test with correct assertion
    // ok shouldEqual firstRs // TxBlob, Hash, and TxnSignatures differ.

    //
    //    val result: JsonObject = signers.foldLeft(firstFakeRs) {
    //      case (rs, fkp) =>
    //        val txjson = getOrLog(findObjectField("result", rs).flatMap(findObjectField("tx_json", _)))
    //        val full   = signFor(txjson, fkp)
    //        full
    //    }
    //
    //    checkResults(result)

    //OTestLogging.setTestLogLevel(Level.Warn)
  }

  test("Sorting") {
    // This is pre-sorted correctly from a Ripple Response message.
    val signersTxt =
      """
        |[
        |                                       {
        |                            "Signer" : {
        |                                "Account" : "rUCwgzripA7QLETs34WDRzNSxdQHsVbnNH",
        |                                "SigningPubKey" : "ED15C69519E06E55B68723E95184B49762205F74ED0426B1653E04570060D46D1B",
        |                                "TxnSignature" : "F42E623A7020FCCBB8D8A5C86065D1D28520E4D64BF28A064DF27E265FEB6458D7C22BE75A7D9EB4450E6E26B99957F28972AF76DACCE7DAB8EEA796E109B107"
        |                            }
        |                        },
        |                        {
        |                            "Signer" : {
        |                                "Account" : "rNtXSGAt7hHvu4ptX5r5kooHe9A39oWXGf",
        |                                "SigningPubKey" : "ED89490B92E78F919E7ED2975427040F74692A2D437B19310D58BBC417B019080A",
        |                                "TxnSignature" : "56D29E42911F354649674197151E493DBD4F65DAE92D040398F24987739BCE87278E241ACD76E0A08975D2B59BD4C9F93FDEA027BE6931E1AB50408341DA4F0D"
        |                            }
        |                        },
        |                        {
        |                            "Signer" : {
        |                                "Account" : "rGSBkMNUcUgA8LfYVzQnDMYx4hsK8wD4ib",
        |                                "SigningPubKey" : "EDAA9D50655A392443A2027C382E4FD92B6072648CEEE896DFBAE02A890C718A4E",
        |                                "TxnSignature" : "125FA71D1D692AEDD946970ECE580AA46A80260196E1BA20243965C92226D1604962B24DA8F59B70FD18C3292A888BD0459AC0E340174E11BEEBD1869F921A02"
        |                            }
        |                        },
        |                        {
        |                            "Signer" : {
        |                                "Account" : "rGXwaPmMxoiddjyWtgnSBCS2SaedReqNf2",
        |                                "SigningPubKey" : "EDDFFFDC9FC4601956DC07143CDDA20C5C2D641C50C138256B5FA1B62F3A30704E",
        |                                "TxnSignature" : "4B15BBC4230873454145C6528EF8DE5795E8EFD8D0434F1730B9BAEC4E4AD96FB0E11472EED2DF52C5F7108045D84450BDDA3045D4CF1DB3D996352EFE105200"
        |                            }
        |                        }
        |                    ]""".stripMargin

    val json             = getOrLog(parseAsJson(signersTxt))
    val signers: Signers = getOrLog(decode(json, Decoder[Signers]))

    val expected: List[JsonObject] = getOrLog(decode(json, Decoder[List[JsonObject]]))

    expected.sortBy(signerSortBy) shouldEqual expected

    // Okay, lets sort a few more in differint order.
    signers.signers.permutations.foreach { lst: List[Signer] =>
      val json: List[JsonObject] = lst.map(_.asJsonObject)
      json.sortBy(signerSortBy)
    }

  }
}
