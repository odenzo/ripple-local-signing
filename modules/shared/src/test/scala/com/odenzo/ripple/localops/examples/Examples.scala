package com.odenzo.ripple.localops.examples

import java.util.UUID

import io.circe.JsonObject
import scribe.Logging

import com.odenzo.ripple.localops.impl.utils.CirceUtils
import com.odenzo.ripple.localops.impl.utils.caterrors.CatsTransformers.ErrorOr
import com.odenzo.ripple.localops.{RippleLocalAPI, SECP256K1}

object Examples extends App with RippleLocalAPI with Logging {

  def makeNewAccount() = {

    for {
      // Example of using an existing account and getting a signing key.
      // We use Genesis account here
      funderSigningKey <- precomputeSigningKey("snoPBrXtMeMyMHUVTgbuqAfg1SUTb", SECP256K1)
      funderAddr = "rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh"

      keys <- generateKeys(SECP256K1)
      (masterKey, regKey) = keys

      // Now make a payment transaction to activate the account (masterKey)
      sequence = 555 // Have to call the XRPL server to see what next sequence number is. See https://xrpl.org/account_info.html#main_content_body
      tx_json <- makeXrpPayment(funderAddr, masterKey.account_id, sequence)
      signed  <- super.signTxnWithHash(tx_json, funderSigningKey)
      (blob, hash) = signed
      submit       = makeSubmit(blob)
      // THen submit this, check for errors, and wait for it to be validated next consensus time.
      // Our account is then activated.

      // Set the regualary key
      masterSigningKey <- precomputeSigningKey(masterKey.master_seed, masterKey.key_type)
      setReg           <- makeSetRegularKey(masterKey.account_id, regKey.account_id, sequence + 1)
      signedSet        <- super.signTxnWithHash(setReg, masterSigningKey)
      // submit.....
    } yield (masterKey.master_seed, regKey.master_seed)

  }

  /**
    *
    * @param fromAddr
    * @param toAddr
    *
    * @return The tx_json portion of a payment request for signing and submitting
    */
  def makeXrpPayment(fromAddr: String, toAddr: String, sequence: Int): ErrorOr[JsonObject] = {
    val obj =
      s"""
         | {
         |    "TransactionType" : "Payment",
         |    "Account" : "$fromAddr",
         |    "Destination" : "$toAddr",
         |    "Amount" : "100000000",
         |    "Fee" : "100"
         |    "Flags" : "2147483648"    ,
         |    "Sequence" : $sequence
         |
         | }
      """.stripMargin

    CirceUtils.parseAsJsonObject(obj)
  }

  def makeSubmit(tx_blob: String): ErrorOr[JsonObject] = {
    val obj = s"""
                 | {
                 |    "id" = "${UUID.randomUUID()}",
                 |    "command" = "submit",
                 |    "tx_blob" = "$tx_blob",
                 |    "fail_hard" = "true"
                 | }
      """.stripMargin

    CirceUtils.parseAsJsonObject(obj)
  }

  def makeSetRegularKey(address: String, regularAddress: String, seq: Int): ErrorOr[JsonObject] = {
    val obj =
      s"""
         | {
         |    "TransactionType" : "SetRegularKey",
         |    "Account" : "$address",
         |    "RegularKey" : "$regularAddress",
         |    "Fee" : "100"
         |    "Flags" : "0"    ,
         |    "Sequence" : $seq
         | }
      """.stripMargin

    CirceUtils.parseAsJsonObject(obj)
  }

}
