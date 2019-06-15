package com.odenzo.ripple.localops.crypto

import java.security.KeyPair

import com.odenzo.jjutils.RFC1751Java
import com.odenzo.ripple.localops.crypto.AccountFamily.{hex2ulong, ripemd160, sha256, sha512}
import com.odenzo.ripple.localops.crypto.core.Secp256K1CryptoBC
import com.odenzo.ripple.localops.utils.{ByteUtils, RBase58}
import com.odenzo.ripple.localops.utils.caterrors.{AppError, AppException}

trait RippleFormatConverters {


  /** This is Base16 format - Doesn't matter key_type */
  def passphase2seed(password: String): Seq[Byte] = sha512(password.getBytes("UTF-8")).take(16)


  /**
    * @param publicKey secp265k or ed25519 keys key, if ed25519 padded with 0xED
    *
    * @return Ripple Account Address Base58 encoded with leading r
    *
    */
  def accountpubkey2address(publicKey: Seq[Byte]): String = {
    assert(publicKey.length == 32 || publicKey.length == 33)

    // Should start with ED if 32 byte  Ed25519
    val sha: Seq[Byte] = sha256(publicKey)
    val accountId: Seq[Byte] = ripemd160(sha)
    val payload: Seq[Byte] = 0.toByte +: accountId

    val checksumHash1: Seq[Byte] = sha256(payload)
    val checksum = sha256(checksumHash1).take(4)

    val bytes: Seq[Byte] = payload ++ checksum
    val b58: String = RBase58.encode(bytes)
    b58
  }

  /** This trims off the first *byte* and the last four checksum bytes from
    * Base58 Ripple encoded things. Suitable for master seed and public key
    *
    * @param rippleB58
    *
    * @return
    */
  def convertBase58Check2hex(rippleB58: String): Either[AppError, String] = {
    AppException.wrap(s"Converting MasterSeed $rippleB58 to MasterSeedHex"){
      for {
        bytes <- RBase58.decode(rippleB58)
        trimmed = bytes.drop(1).dropRight(4)
        hex  = ByteUtils.bytes2hex(trimmed)
      } yield hex
    }
  }


  /**
    * Suitable for converted WalletPropose secretKey to master_seed_hex
    *
    * @param masterKeyRFC1751
    *
    * @return Pure format conversion from RFC1751 human words to hex seed
    */
  def convertMasterKey2masterSeedHex(masterKeyRFC1751: String): Either[AppError, String] = {
    AppException.wrapPure("RFC1751 to Master Seed Hex"){
      RFC1751Java.getKeyFromEnglish(masterKeyRFC1751)
    }
  }

}

object RippleFormatConverters extends RippleFormatConverters
