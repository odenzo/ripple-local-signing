package com.odenzo.ripple.localops.messagebasedapis

import io.circe.syntax._
import io.circe.{Decoder, Json, JsonObject}

import com.odenzo.ripple.bincodec.testkit.JsonReqRes
import com.odenzo.ripple.localops.impl.messagehandlers.WalletProposeMsg
import com.odenzo.ripple.localops.impl.utils.JsonUtils
import com.odenzo.ripple.localops.models.WalletProposeResult
import com.odenzo.ripple.localops.testkit.{FixtureUtils, OTestSpec}
import com.odenzo.ripple.localops.{LocalOpsError, MessageBasedAPI}

class WalletProposeMsgFixtureTest extends OTestSpec with FixtureUtils {

  /*
      The fixtures here are sending in blank requests (except key type)
      and getting an actual set of account keys.
      This test works on the response, and takes pieces of response keys and creates a request form it.
      The result of the constructed request should be the same as original result.
   */

  //override val customLogLevel = Some(Level.Debug)
  private lazy val ed: Either[LocalOpsError, List[JsonReqRes]] = loadRqRsResource(
    "/test/myTestData/wallets/ed25519_wallets.json"
  )

  private lazy val secp: Either[LocalOpsError, List[JsonReqRes]] = loadRqRsResource(
    "/test/myTestData/wallets/secp256k1_wallets.json"
  )

  test("Each Secret for Each Message") {
    val rr: List[JsonReqRes] = getOrLog(ed) ::: getOrLog(secp)
    rr.foreach(walletproposeRegime)

  }

  test("ID Scenarios") {
    val rrs: List[JsonReqRes] = getOrLog(ed) ::: getOrLog(secp)

    val idsToTest          = List(None, Some(666.asJson), Some("my id".asJson), Some(Json.Null))
    val onCase: JsonReqRes = rrs.head

    idsToTest.foreach { id =>
      walletproposeRegime(
        JsonReqRes(setId(id, onCase.rq), setId(id, onCase.rs))
      )
    }

    // Lets do a few error cases
    rrs.take(1).foreach { rr =>
      val res = WalletProposeMsg.propose(rr.rq).fold(identity, identity).asJson
      logger.debug(s"Random Response: ${res.spaces4}")
      (res.dropNullValues.spaces4) should not equal (rr.rs.asJson.dropNullValues.spaces4)
    }
  }

  test("Bad Request A") {
    val rrs: List[JsonReqRes] = getOrLog(ed)
    rrs.take(1).foreach { rrs =>
      val request                           = rrs.rq.mapObject(_.add("seed", "Garbage".asJson))
      val result                            = MessageBasedAPI.walletPropose(request)
      val st: Either[LocalOpsError, String] = findFieldAsString("status", result)
      getOrLog(st) shouldEqual "error"

    }
  }

  /** Goes through all the possible test cases we can mimic and get the same result. */
  def walletproposeRegime(rr: JsonReqRes): Unit = {

    val result: JsonObject        = findRequiredObject("result", rr.rs)
    val keys: WalletProposeResult = getOrLog(JsonUtils.decode(result, Decoder[WalletProposeResult]))

    // Any one of these can be supplied in the test, so we construct 5 cases from each response

    List(
      "seed"       -> keys.master_seed,
      "seed_hex"   -> keys.master_seed_hex,
      "passphrase" -> keys.master_key,
      "passphrase" -> keys.master_seed,
      "passphrase" -> keys.master_seed_hex
    ).foreach {
      case (fieldName: String, fieldVal: String) =>
        logger.debug(s"Injecting $fieldName => $fieldVal")
        val injected       = rr.rq.mapObject(_.add(fieldName, fieldVal.asJson))
        val response: Json = MessageBasedAPI.walletPropose(injected)
        logger.debug(s"Response:\n${response.asJson.spaces4}")
        response.asJson shouldEqual rr.rs.asJson.dropNullValues
    }
  }

}
