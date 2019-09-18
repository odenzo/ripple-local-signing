package com.odenzo.ripple.localops.impl.messagehandlers

import io.circe.Json

import cats._
import cats.data._
import cats.implicits._
import scribe.Logging

import com.odenzo.ripple.localops.impl.crypto.RippleFormatConverters
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.localops.impl.{BinCodecProxy, Sign, SignFor}
import com.odenzo.ripple.localops.models.ResponseError

/**
  * The details of how this works are not documented, so I am assuming similar to Sign.
  * Basically, the tx_json has a empty ("") string for SigningPubKey.
  * The account address for the transaction has a set of "keys" set to its SignerList.
  *
  * SignFor adds one Signer to the array of Signers.  The Account and SignerPubKey are raw data.
  * The TxnSignature is calculated somehow.
  * I assume it is calculates the same way as a TxnSignature for normal signing case.
  * Testing with server shows that the TxnSignatures are invariant when the order of signing is changed.
  *
  * Remove heavy duplication between this and SignRqRsHandler was working.
  **/
object SignForMsg extends HandlerBase with Logging with JsonUtils with RippleFormatConverters {

  /**
    *
    * @param rq Full SignRq (SingingPubKey should be "" at top level)
    *
    * @return Left is error response SignRs, Right is success response
    */
  def signFor(rq: Json): Either[Json, Json] = {
    logger.debug(s"signFor Rq: \n ${rq.spaces4}")
    val ok: Either[ResponseError, Json] = for {
      jobj        <- ensureJsonObject(rq).leftMap(err => ResponseError.invalid("Rq Not JsonObject"))
      _           <- validateCommandCorrect("sign_for", rq)
      key         <- extractKey(rq) // One of the multisigners.
      signingAcct <- findFieldAsString("account", rq).leftMap(_ => ResponseError.kNoAccount)
      tx_json     <- findField("tx_json", rq).leftMap(_ => ResponseError.kNoTxJson)
      tx_jsonOut  <- SignFor.signFor(tx_json, key, signingAcct).leftMap(_ => ResponseError.kBadSecret)

      txBlob <- BinCodecProxy.serialize(tx_jsonOut).leftMap(err => ResponseError.internalErr(err.msg))
      blobhex = ByteUtils.bytes2hex(txBlob)
      hash    = Sign.createResponseHashHex(txBlob)
      optId   = findField("id", rq).toOption
      result  = buildTxnSuccessResultField(tx_jsonOut, blobhex, hash)
      success = buildSuccessResponse(result, optId)
    } yield success

    ok.leftMap(re => buildFailureResponse(rq, re))

  }

}
