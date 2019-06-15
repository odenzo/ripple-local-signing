package com.odenzo.ripple.localops.reference

import com.typesafe.scalalogging.StrictLogging
import spire.math.{UByte, UInt}

import com.odenzo.ripple.localops.utils.ByteUtils
import com.odenzo.ripple.localops.utils.caterrors.AppError

import cats._
import cats.data._
import cats.implicits._

case class HashPrefix(v: List[UByte]) {

  val asHex  : String      = ByteUtils.ubytes2hex(v)
  val asBytes: Array[Byte] = v.map(_.toByte).toArray
}

/** These are all Four Bytes Long with bottom byte 00  */
object HashPrefix extends StrictLogging {

  // Unsigned Transaction is "53545800 + TxBlob sha512half
  // Unsigned Multisigned
  // Signed "54584E00 (both kinds I think)
  /** For after a signed (and? multisignd) txn is signed */
  val transactionID: HashPrefix = toHashPrefix("54584E00")
  // For unsigned single signer txn
  val transaction: HashPrefix = toHashPrefix("534E4400")
  // account state
  val accountStateEntry: HashPrefix = toHashPrefix("4D4C4E00")
  // inner node in tree
  val innerNode: HashPrefix = toHashPrefix("4D494E00")
  // ledger master data for signing
  val ledgerHeader: HashPrefix = toHashPrefix("4C575200")


  /**  inner transaction to single signed, before signing */
  val transactionSig: HashPrefix = toHashPrefix("53545800")

  // inner transaction to sign
  val transactionMultiSig: HashPrefix = toHashPrefix("534D5400")
  // validation for signing
  val validation: HashPrefix = toHashPrefix("56414C00")
  // proposal for signing
  val proposal: HashPrefix = toHashPrefix("50525000")
  // payment channel claim
  val paymentChannelClaim: HashPrefix = toHashPrefix("434C4D00")

  /** None of these overflow signed I think */
  def toHashPrefix(lhex: String): HashPrefix = {
    val ubytes: List[UByte] = ByteUtils.unsafeHex2ubytes(lhex)
    HashPrefix(ubytes)
  }
}
//
//object RippledHashPrefix {
//
//  val transactionID: UInt       = HashPrefix('T', 'X', 'N')
//  val txNode: UInt              = HashPrefix('S', 'N', 'D')
//  val leafNode: UInt            = HashPrefix('M', 'L', 'N')
//  val innerNode: UInt           = HashPrefix('M', 'I', 'N')
//  val innerNodeV2: UInt         = HashPrefix('I', 'N', 'R')
//  val ledgerMaster: UInt        = HashPrefix('L', 'W', 'R')
//  val txSign: UInt              = HashPrefix('S', 'T', 'X')
//  val txMultiSign: UInt         = HashPrefix('S', 'M', 'T')
//  val validation: UInt          = HashPrefix('V', 'A', 'L')
//  val proposal: UInt            = HashPrefix('P', 'R', 'P')
//  val manifest: UInt            = HashPrefix('M', 'A', 'N')
//  val paymentChannelClaim: UInt = HashPrefix('C', 'L', 'M')
//
//
//}
