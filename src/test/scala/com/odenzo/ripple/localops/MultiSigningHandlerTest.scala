package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Decoder, Json, JsonObject}
import org.scalatest.FunSuite
import scribe.Level

import com.odenzo.ripple.localops.handlers.SignForRqRsHandler
import com.odenzo.ripple.localops.testkit.{AccountKeys, FixtureUtils, OTestLogging, OTestSpec}
import com.odenzo.ripple.localops.utils.caterrors.AppError
import com.odenzo.ripple.localops.utils.{ByteUtils, CirceUtils}

class MultiSigningHandlerTest extends FunSuite with OTestSpec with ByteUtils with FixtureUtils {

  // Account key and 5 multsig keys (unactivated addresses)
  private val allKeys =
    """
      |
      |
      |[
      |    {
      |        "master" : {
      |            "account_id" : "rPHcVF8zfgE2hL2fSxSg4LsG4zsy1banXu",
      |            "key_type" : "ed25519",
      |            "master_key" : "TUM WHAM LUSH MOLT WAD GAUL HECK WALT FLAK HERS SODA WINE",
      |            "master_seed" : "sa9m2xo8iJthwaT6KSBxnSr9oq89P",
      |            "master_seed_hex" : "F817AE4B07DAFE95133FF45ED3529F42",
      |            "public_key" : "aKEikZTjZH7azaG3WoZZjMmwykc9R36TMaC6kJm8MP7czpg6mqBM",
      |            "public_key_hex" : "ED8FC20335C7385769116C82A9020D36AA57182AF4E59F80721FF3CC758C2081B8"
      |        },
      |        "regular" : null
      |    },
      |    {
      |        "master" : {
      |            "account_id" : "raaWTYxreqTZUbqt1vWypPt5jVqvKXSM9u",
      |            "key_type" : "ed25519",
      |            "master_key" : "WED LOSS AMOS HONK VIEW LEON TIED SAUL LOS RILL HID CUFF",
      |            "master_seed" : "snjzwvYyFc9sKnzS1MsJN5fJpQPHf",
      |            "master_seed_hex" : "DAA8D1E796E07AEB5A4F1F4D325DD644",
      |            "public_key" : "aKG7ijoqd8nCXAjLZnGC5sj5cUmC8iBnZ5VFgMmYSgscnoC6yFAq",
      |            "public_key_hex" : "EDE1BDFCF6CF88A81BE0E352B1DF337F3C7946AE0F89FD332BD68311D064E049AC"
      |        },
      |        "regular" : null
      |    },
      |    {
      |        "master" : {
      |            "account_id" : "rHmyZx9V8iCKqRexJ12cABJ7GosHeb1gSM",
      |            "key_type" : "ed25519",
      |            "master_key" : "PET MESH TOGO IVY BANE SIS GILD DEAD BET BOSE BOB CAFE",
      |            "master_seed" : "snSnQK1Y1m3yyc8LCv8caV7LEyHM7",
      |            "master_seed_hex" : "C47EB02E1C144E8B7822858FB3535733",
      |            "public_key" : "aKEpeM4nytz1jiBucbuC2BwSbwB7ekqQ1q777z4Q7Lrnma4b7tJt",
      |            "public_key_hex" : "ED228661F2E5D01453E7599DBF9CEC998D47B411B8E0C17C5CC54E0A600D70E9FB"
      |        },
      |        "regular" : null
      |    },
      |    {
      |        "master" : {
      |            "account_id" : "r4fZJpejq36jdFo6rN8XygVSHMfo1RUryM",
      |            "key_type" : "ed25519",
      |            "master_key" : "JUDD QUOD TOM AMRA COLT PAY OMEN TIDY TOOT TIN SAUL DEFT",
      |            "master_seed" : "sn1neGkyBQFZ58k7UmmnJuCHnyaLZ",
      |            "master_seed_hex" : "E3706D20B86B5DC865A056A6056139A3",
      |            "public_key" : "aKEYrg1jgSKPFhUfrQP2QnYjnNKqW9nnDxrzHshnkq2W9ocYpLjm",
      |            "public_key_hex" : "ED6535514370CF87B08C05A7AD0A92C967C1EC4EFA551AED495F455526949C7F75"
      |        },
      |        "regular" : null
      |    },
      |    {
      |        "master" : {
      |            "account_id" : "rhVXRNaYRZJp9DLkRcCYr7eYcZprCBgPBw",
      |            "key_type" : "ed25519",
      |            "master_key" : "TOLL HID DUD LAWS BEST LORE CURE OTT SEAR ANNA VEIL LOAM",
      |            "master_seed" : "shhgx6euKKTmMvXiiEb9C8dq4UF1V",
      |            "master_seed_hex" : "623F8FA6600BE66D656FC5D53F5043ED",
      |            "public_key" : "aKEbTjXdPFpzZXUTTanVH5s37WVAqTEpL7WgxUZdviPj7xAWWk4i",
      |            "public_key_hex" : "ED6D0A2FF61FF0251D0C3B0D09AA2D6A7E9ACE2481516FBDAEC03619E384B2180E"
      |        },
      |        "regular" : null
      |    }
      |]
    """.stripMargin

  private val tx_json =
    """{
      |    "TransactionType" : "Payment",
      |    "Account" : "rPHcVF8zfgE2hL2fSxSg4LsG4zsy1banXu",
      |    "Amount" : "10000000",
      |    "Destination" : "rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh",
      |    "InvoiceID" : null,
      |    "Paths" : null,
      |    "SendMax" : null,
      |    "DeliverMin" : null,
      |    "Memos" : null,
      |    "Sequence" : 2,
      |    "LastLedgerSequence" : null,
      |    "Fee" : "50000000",
      |    "Flags" : 2147483648,
      |    "SigningPubKey" : "",
      |    "Signers" : null
      |}""".stripMargin

  private val response =
    """{
      |    "Account" : "rPHcVF8zfgE2hL2fSxSg4LsG4zsy1banXu",
      |    "Amount" : "10000000",
      |    "Destination" : "rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh",
      |    "Fee" : "50000000",
      |    "Flags" : 2147483648,
      |    "Sequence" : 2,
      |    "Signers" : [
      |        {
      |            "Signer" : {
      |                "Account" : "rhVXRNaYRZJp9DLkRcCYr7eYcZprCBgPBw",
      |                "SigningPubKey" : "ED6D0A2FF61FF0251D0C3B0D09AA2D6A7E9ACE2481516FBDAEC03619E384B2180E",
      |                "TxnSignature" : "B6AEFB9E347CBD5D6A4BA90BF694F185E3A82A9F2B9ABB816E80AF5254F7E8653462DBD8F3673305521DA47B10A46FF8F39DA912A388637A3081FEE6FCF52A08"
      |            }
      |        },
      |        {
      |            "Signer" : {
      |                "Account" : "raaWTYxreqTZUbqt1vWypPt5jVqvKXSM9u",
      |                "SigningPubKey" : "EDE1BDFCF6CF88A81BE0E352B1DF337F3C7946AE0F89FD332BD68311D064E049AC",
      |                "TxnSignature" : "9B572194AC3689B740CB0EBE70B5E0BCF5EEE0CF6A364E36645A0C26A20880EA286738E24A820A6FFD4EC11CE45D69ABC71176A220491D734EEE19556018F10A"
      |            }
      |        },
      |        {
      |            "Signer" : {
      |                "Account" : "rHmyZx9V8iCKqRexJ12cABJ7GosHeb1gSM",
      |                "SigningPubKey" : "ED228661F2E5D01453E7599DBF9CEC998D47B411B8E0C17C5CC54E0A600D70E9FB",
      |                "TxnSignature" : "11B22084FE67747D7F3179C4A88FF06AEB602390BD4DEBCE52D379C5CFD879B30A7A0BD41800AF8FE5196E32DA1F7F477538B5056245E545D100AB2286769C09"
      |            }
      |        },
      |        {
      |            "Signer" : {
      |                "Account" : "r4fZJpejq36jdFo6rN8XygVSHMfo1RUryM",
      |                "SigningPubKey" : "ED6535514370CF87B08C05A7AD0A92C967C1EC4EFA551AED495F455526949C7F75",
      |                "TxnSignature" : "0BB4A0AD3B9E00AD84D10820D98F100C3DAD3583852C19193AB25A23C448522425E1B175AE5F0D5AD13DFF64CB20210F143D05DB3C20A222D3BF01EF2E082005"
      |            }
      |        }
      |    ],
      |    "SigningPubKey" : "",
      |    "TransactionType" : "Payment",
      |    "hash" : "6FB3558A94076CD812E8390EA4379DCA6DB8A66C35F5FDE0B5A47A84F5BC8880"
      |}
      |""".stripMargin

  case class FKP(master: AccountKeys, regular: Option[AccountKeys])
  object FKP {
    implicit val decoder: Decoder[FKP] = deriveDecoder[FKP]
  }
  test("signFor TDP") {
    OTestLogging.setTestLogLevel(Level.Debug)
    val keyListAttempt: Either[AppError, List[FKP]] = CirceUtils.parseAndDecode(allKeys, Decoder[List[FKP]])
    val kl: List[FKP]                               = getOrLog(keyListAttempt)

    logger.debug(s"Key Pairs:\n ${kl.map(_.toString).mkString("\n")}")

    val tx: JsonObject = getOrLog(CirceUtils.parseAsJsonObject(tx_json))
    val fkp: FKP       = kl.head

    val rq = JsonObject(
      "id"       := "First",
      "command"  := "sign_for",
      "account"  := fkp.master.address,
      "seed_hex" := fkp.master.master_seed_hex,
      "key_type" := fkp.master.key_type,
      "tx_json"  := tx
    )

    val rqTrimmed = CirceUtils.pruneNullFields(rq)

    logger.info("Request Object (First): " + rq.asJson.spaces4)
    SignForRqRsHandler.signFor(rqTrimmed) match {
      case Right(ok) ⇒ logger.debug(s"SignFor OK: ${ok.asJson.spaces4}")
      case Left(err) ⇒
        logger.warn(s"Error Response ${err.asJson.spaces4}")
        assert(false, "SignFor Failed")
    }

    OTestLogging.setTestLogLevel(Level.Warn)
  }

}
