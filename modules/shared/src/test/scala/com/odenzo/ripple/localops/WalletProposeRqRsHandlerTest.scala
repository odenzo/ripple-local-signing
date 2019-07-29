package com.odenzo.ripple.localops

import io.circe.syntax._
import io.circe.{Decoder, Json, JsonObject}
import org.scalatest.BeforeAndAfter

import com.odenzo.ripple.localops.impl.messagehandlers.WalletProposeRqRsHandler
import com.odenzo.ripple.localops.impl.utils.JsonUtils
import com.odenzo.ripple.localops.testkit.{FixtureUtils, OTestSpec}

class WalletProposeRqRsHandlerTest extends OTestSpec with FixtureUtils with BeforeAndAfter {

  lazy val ed   = loadRqRsResource("/test/myTestData/keysAndTxn/ed25519_wallets.json")
  lazy val secp = loadRqRsResource("/test/myTestData/keysAndTxn/secp256k1_wallets.json")

  before {
    //setTestLogLevel(Level.Debug)
  }

  after {
    //  setTestLogLevel(Level.Info)
  }

  test("all") {

    val rr: List[(JsonObject, JsonObject)] = getOrLog(ed) ::: getOrLog(secp)

    // We take the request, which has no seed value in it, and inject
    // a "result" seed which can be master_seed_hex, master_keys, master_seed
    // Just cannot test "pure random" or a password based approach.

    rr.foreach {
      case (rq: JsonObject, rs: JsonObject) => testAllGood(rq, rs)
    }

    // Make sure non-string and also no id works ok. id = null will be returned in object,
    // but serialized with non-null printer is ok ( "x" = null   =!=  no field x present)
    rr.take(1).foreach {
      case (rq, rs) =>
        val noId = rq.remove("id")
        testAllGood(noId, rs.remove("id").add("id", Json.Null))
        testAllGood(rq.add("id", 666.asJson), rs.add("id", 666.asJson))
    }

    // Lets do a few error cases
    rr.take(1).foreach {
      case (rq, rs) =>
        val res = WalletProposeRqRsHandler.propose(rq)
        logger.debug(s"Random Response: ${res.asJson.spaces4}")
        res should not equal rs
    }
  }

  /** Goes through all the possible test cases we can mimic and get the same result. */
  def testAllGood(rq: JsonObject, rs: JsonObject): Unit = {

    val result: JsonObject        = findRequiredObject("result", rs)
    val keys: WalletProposeResult = getOrLog(JsonUtils.decode(result, Decoder[WalletProposeResult]))

    List(
      "seed"       -> keys.master_seed,
      "seed_hex"   -> keys.master_seed_hex,
      "passphrase" -> keys.master_key,
      "passphrase" -> keys.master_seed,
      "passphrase" -> keys.master_seed_hex
    ).foreach {
      case (fieldName: String, fieldVal: String) =>
        logger.debug(s"Injecting $fieldName => $fieldVal")
        val injected             = rq.add(fieldName, fieldVal.asJson)
        val response: JsonObject = WalletProposeRqRsHandler.propose(injected)
        logger.debug(s"Response:\n${response.asJson.spaces4}")
        response shouldEqual rs
    }
  }

}
