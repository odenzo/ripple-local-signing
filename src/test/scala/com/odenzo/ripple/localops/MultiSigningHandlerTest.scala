package com.odenzo.ripple.localops

import java.util.UUID

import cats._
import cats.data._
import cats.implicits._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Decoder, JsonObject}
import org.scalatest.FunSuite
import scribe.Level

import com.odenzo.ripple.bincodec.Decoded
import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import com.odenzo.ripple.bincodec.syntax.debugging._
import com.odenzo.ripple.localops.impl.messagehandlers.SignForRqRsHandler
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, CirceUtils}
import com.odenzo.ripple.localops.testkit.{AccountKeys, FixtureUtils, JsonReqRes, OTestLogging, OTestSpec}

class MultiSigningHandlerTest extends FunSuite with OTestSpec with ByteUtils with FixtureUtils {
  val tracer =
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

    val rqTrimmed = CirceUtils.pruneNullFields(rq)

    logger.info("Request Object (First): " + rqTrimmed.asJson.spaces4)
    val rs             = SignForRqRsHandler.signFor(rqTrimmed)
    val ok: JsonObject = rs.right.value
    ok
  }

  //
  /**
    *
    * @param got      SignRs full that we calculated
    * @param expected SignRs full from trace that is should equal
    *
    * @return
    */
  private def checkResults(got: JsonObject, expected: JsonObject): Unit = {

    val expResult        = getOrLog(findObjectField("result", expected))
    val exTxBlob: String = getOrLog(findStringField("tx_blob", expResult))

    val cResultObj      = getOrLog(findObjectField("result", got))
    val cTxBlob: String = getOrLog(findStringField("tx_blob", cResultObj))

    logger.debug(s"Got  vs Expected Blob Len: ${cTxBlob.length} and Got ${exTxBlob.length}")

    logger.debug(s"Got vs Expected Blob \n $cTxBlob \n $exTxBlob")

    val target: List[Decoded] = getOrLog(TxBlobBuster.bust(exTxBlob).leftMap(e ⇒ AppError(s"Busting ${e.msg}")))

    logger.debug(s"Got vs Targert Str: \n $cTxBlob \n $exTxBlob")
    val gotEnc = getOrLog(TxBlobBuster.bust(cTxBlob).leftMap(e ⇒ AppError(s"Busting ${e.msg}")))

    gotEnc.foreach(dec ⇒ logger.debug(s"TxBlob Got    Field: " + dec.show))
    target.foreach(dec ⇒ logger.debug(s"TxBlob Target Field: " + dec.show))
    // TODO: Fix this assertion
    // got shouldEqual expected.result.remove("deprecated")

  }

  test("signFor TDP") {
    OTestLogging.setTestLogLevel(Level.Debug)
    val trace: MultiSignTrace = getOrLog(CirceUtils.parseAndDecode(tracer, MultiSignTrace.decoder))

    val firstRq        = getOrLog(json2object(trace.signFors.head.rq))
    val firstRs        = getOrLog(json2object(trace.signFors.head.rs))
    val rs             = SignForRqRsHandler.signFor(firstRq)
    val ok: JsonObject = rs.right.value

    checkResults(ok, firstRs)

    logger.info(s"Result ${ok.asJson.spaces4}")
    // TODO: Complete this test with correct assertion
    // ok shouldEqual firstRs // TxBlob, Hash, and TxnSignatures differ.

    //
    //    val result: JsonObject = signers.foldLeft(firstFakeRs) {
    //      case (rs, fkp) ⇒
    //        val txjson = getOrLog(findObjectField("result", rs).flatMap(findObjectField("tx_json", _)))
    //        val full   = signFor(txjson, fkp)
    //        full
    //    }
    //
    //    checkResults(result)

    OTestLogging.setTestLogLevel(Level.Warn)
  }

  // Always tracing these with master key only.
  case class MultiSignTrace(account: AccountKeys, signers: List[AccountKeys], signFors: List[JsonReqRes])

  case class FKP(master: AccountKeys, regular: Option[AccountKeys])

  object MultiSignTrace {

    import io.circe.generic.semiauto._

    implicit val decoder: Decoder[MultiSignTrace] = deriveDecoder[MultiSignTrace]
  }

  object FKP {
    implicit val decoder: Decoder[FKP] = deriveDecoder[FKP]
  }

}
