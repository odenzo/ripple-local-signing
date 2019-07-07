package com.odenzo.ripple.localops.crypto

import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.localops.crypto.AccountFamily.{ripemd160, sha256, sha512}
import com.odenzo.ripple.localops.crypto.core.HashOps
import com.odenzo.ripple.localops.utils.caterrors.{AppError, AppException}
import com.odenzo.ripple.localops.utils.{ByteUtils, RippleBase58}

trait RippleFormatConverters {

  /**
    * @param publicKey secp265k or ed25519 keys key, if ed25519 padded with 0xED
    *
    * @return Ripple Account Address Base58 encoded with leading r
    *
    */
  def accountpubkey2address(publicKey: Seq[Byte]): String = {
    assert(publicKey.length === 32 || publicKey.length === 33)

    // Should start with ED if 32 byte  Ed25519
    val sha: Seq[Byte]       = sha256(publicKey)
    val accountId: Seq[Byte] = ripemd160(sha)
    val payload: Seq[Byte]   = 0.toByte +: accountId

    val checksumHash1: Seq[Byte] = sha256(payload)
    val checksum                 = sha256(checksumHash1).take(4)

    val bytes: Seq[Byte] = payload ++ checksum
    val b58: String      = RippleBase58.encode(bytes)
    b58
  }

  /** TODO: Check this. */
  def convertPassphrase2hex(password: String): Either[AppError, String] = {
    ByteUtils.bytes2hex(sha512(password.getBytes("UTF-8")).take(16)).asRight[AppError]
  }

  /** This trims off the first *byte* and the last four checksum bytes from
    * Base58 Ripple encoded things. Suitable for master seed and public key
    *
    * @param rippleB58
    *
    * @return
    */
  def convertBase58Check2hex(rippleB58: String): Either[AppError, String] = {
    AppException.wrap(s"Converting MasterSeed $rippleB58 to MasterSeedHex") {
      for {
        bytes   <- RippleBase58.decode(rippleB58)
        trimmed = bytes.drop(1).dropRight(4)
        hex     = ByteUtils.bytes2hex(trimmed)
      } yield hex
    }
  }

  val accountPrefix          = "00"
  val publicKeyPrefix        = "23"
  val seedValuePrefix        = "21"
  val validationPubKeyPrefix = "1C"

  /** You will have to add prefix per https://xrpl.org/base58-encodings.html */
  def convertHex2seedB58Check(prefix: String, hex: String) = {
    // This is pure conversion, no family stuff. Adds s and checksum

  }

  /**
    *
    * @param bytes 33 bytes (OD+32 bytes for ed25519)
    *              @return Base58 checksum encoded
    */
  def publicKeyToAddress(bytes: List[Byte]) = {
    val accountId = HashOps.sha256(HashOps.ripemd160(bytes))
    val checksum  = HashOps.sha256(HashOps.sha256(accountId))
    val address   = RippleBase58.encode(accountId ++ checksum)
    address
  }

  /**
    * Suitable for converted WalletPropose secretKey to master_seed_hex
    *
    * @param masterKeyRFC1751
    *
    * @return Pure format conversion from RFC1751 human words to hex seed
    */
  def convertMasterKey2masterSeedHex(masterKeyRFC1751: String): Either[AppError, String] = {
    AppException.wrap("RFC1751 to Master Seed Hex") {
      RFC1751Keys.getKeyFromTwelveWords(masterKeyRFC1751)
    }
  }

}

object RippleFormatConverters extends RippleFormatConverters
