package com.odenzo.ripple.localops.impl.reference

import cats._
import cats.data._
import scribe.Logging
import spire.math.UByte

import com.odenzo.ripple.localops.impl.utils.ByteUtils

case class HashPrefix(v: List[UByte]) {

  val asBytes: IndexedSeq[Byte] = v.map(_.toByte).toIndexedSeq
  val asByteArray: Array[Byte]  = v.map(_.toByte).toArray
  val asHex: String             = ByteUtils.bytes2hex(this.asBytes)
}

/** These are all Four Bytes Long with bottom byte 00  */
object HashPrefix extends Logging {

  // Unsigned Transaction is "53545800 + TxBlob sha512half
  // Unsigned Multisigned
  // Signed "54584E00 (both kinds I think)
  /** Prefix to txblob to make transaction hash */
  val transactionID: HashPrefix = fromHex("54584E00")
  // For unsigned single signer txn
  val transaction: HashPrefix = fromHex("534E4400")
  // account state
  val accountStateEntry: HashPrefix = fromHex("4D4C4E00")
  // inner node in tree
  val innerNode: HashPrefix = fromHex("4D494E00")
  // ledger master data for signing
  val ledgerHeader: HashPrefix = fromHex("4C575200")

  /**  inner transaction to single signed, before signing */
  val transactionSig: HashPrefix = fromHex("53545800")

  /** Transaction for MultiSigning */
  val transactionMultiSig: HashPrefix = fromHex("534D5400")

  /**  validation for signing (validator?) */
  val validation: HashPrefix = fromHex("56414C00")

  /**  proposal for signing, not sure what kind of porposal */
  val proposal: HashPrefix = fromHex("50525000")

  /* payment channel claim */
  val paymentChannelClaim: HashPrefix = fromHex("434C4D00")

  /** None of these overflow signed I think */
  def fromHex(lhex: String): HashPrefix = {
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
