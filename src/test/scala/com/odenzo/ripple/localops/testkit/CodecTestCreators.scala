package com.odenzo.ripple.localops.testkit

import java.security.MessageDigest

import io.circe.Json

import cats._
import cats.data._
import cats.implicits._
import scribe.Logging

import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import com.odenzo.ripple.bincodec.reference.HashPrefix
import com.odenzo.ripple.bincodec.utils.ByteUtils
import com.odenzo.ripple.bincodec.{BinCodecLibError, EncodedSTObject, RippleCodecDebugAPI}

trait CodecTestCreators extends Logging with OTestUtils {

  /** This is the standard way to test encoding of a tx_json for a signrs
    *
    * @param reply  The tope level object in reply (id level). Traverses to result and gets infso
    */
  def checkTxBlob(reply: Json): Either[Throwable, Boolean] = {

    for {

      txjson        <- findTxJsonInReply(reply)
      kTxBlob       <- findTxBlobInReply(reply)
      txBlobEncoded <- RippleCodecDebugAPI.binarySerialize(txjson)
      txBlobHex = txBlobEncoded.toHex
      passFail <- compareTxBlobs(kTxBlob, txBlobHex, txBlobEncoded)
    } yield passFail
  }

  /**
    *
    * @param jobj AN object with a hash field in it, tx_json or ledgertxn etc.
    * @return
    */
  def checkHash(jobj: Json): Either[Throwable, Json] = {

    for {
      kHash <- findField("hash", jobj).flatMap(json2string)
      hash  <- createResponseHashHex(jobj)
      passed <- if (hash === kHash) jobj.asRight
      else {
        logger.warn(s"Hash Not Correct, Got vs Expected: \n$hash \n$kHash")
        RippleCodecDebugAPI.binarySerialize(jobj).foreach(v => logger.warn(s"ENcoding Was ${v.show}"))
        BinCodecLibError(s"Hash Computation Mismatch \n$hash \n$kHash").asLeft
      }

    } yield passed
  }

  /**
    *
    * @param rsObj tx_json or a LedgerTxn, anything with a hash field I think. But hardcoded to tranasctionID prefix
    * @return
    */
  def createResponseHashHex(rsObj: Json): Either[BinCodecLibError, String] = {
    BinCodecLibError.handlingM("Creating Hash Failed") {
      RippleCodecDebugAPI.binarySerialize(rsObj).map { serialized =>
        // logger.info(s"Raw:\n${rsObj.asJson.spaces4}")
        // logger.info(s"BinarySerialized for Hash:\n ${serialized.show}")
        val payload: Seq[Byte]    = HashPrefix.transactionID.asBytes ++ serialized.toBytes
        val digest: MessageDigest = MessageDigest.getInstance("SHA-512")
        digest.update(payload.toArray)
        val fullHash: Array[Byte] = digest.digest()
        val rHash                 = fullHash.take(32)
        ByteUtils.bytes2hex(rHash)
      }
    }
  }

  def compareTxBlobs(expected: String, got: String, encoded: EncodedSTObject): Either[BinCodecLibError, Boolean] = {
    (expected == got) match {
      case true =>
        logger.info("TxBlobs Matched")
        true.asRight
      case false =>
        logger.warn(s"Got  vs Expected Blob Len: ${got.length} and Got ${expected.length}")
        logger.warn(s"Got vs Expected Blob \n $got \n $expected")
        logger.warn(s"My Encoded Version:\n ${encoded.show}")
        for { // Now see what other details we can provide
          exEnc <- TxBlobBuster.bust(expected)
          _ = logger.info(s"TxBlob Expected Field: ${exEnc.show}")
          gotEnv <- TxBlobBuster.bust(got)
          _ = logger.info(s"TxBlob Got      Field: ${gotEnv.show}")

        } yield false
    }
  }

  def dumpEncoded(st: EncodedSTObject): Unit = {
    logger.warn(s"Encoded: ${st.show}")
  }
}

object CodecTestCreators extends CodecTestCreators
