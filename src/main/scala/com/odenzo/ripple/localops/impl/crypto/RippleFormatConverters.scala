package com.odenzo.ripple.localops.impl.crypto

import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.localops.LocalOpsError
import com.odenzo.ripple.localops.impl.crypto.AccountFamily.{ripemd160, sha256, sha512}
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, Hex, RippleBase58}
import com.odenzo.ripple.localops.models.Base58Check

trait RippleFormatConverters {

  val accountPrefix          = Hex("00")
  val publicKeyPrefix        = Hex("23")
  val seedValuePrefix        = Hex("21")
  val validationPubKeyPrefix = Hex("1C")

  /**
    * @param publicKey secp265k or ed25519 keys key, if ed25519 padded with 0xED
    *
    * @return Ripple Account Address Base58 encoded with leading r
    *
    */
  def accountpubkey2address(publicKey: Seq[Byte]): Either[LocalOpsError, Base58Check] = {
    // Should start with ED if 32 byte  Ed25519
    val accountId: IndexedSeq[Byte]                      = ripemd160(sha256(publicKey.toArray).toArray)
    val prefix: Either[LocalOpsError, Byte]              = ByteUtils.hex2byte(accountPrefix.v)
    val payload: Either[LocalOpsError, IndexedSeq[Byte]] = prefix.map(_ +: accountId)
    payload.map(base58Checksum)

  }

  /**
    * Password typically should not be used. Generate random and note the RFC-1751
    *
    * @param password Random password, this routine doesn't warn about low entropy
    *
    * @return 16 bytes in hex form suitable to use as master_seed_hex (if password is good)
    */
  def convertPassword2hex(password: String): Either[LocalOpsError, String] = {
    convertPassword2bytes(password).map(v => ByteUtils.bytes2hex(v.toIterable))
  }

  /**
    * Password typically should not be used. Generate random and note the RFC-1751
    *
    * @param password Random password, this routine doesn't warn about low entropy
    *
    * @return 16 bytes suitable to use as master_seed_hex (if password is good)
    */
  def convertPassword2bytes(password: String): Either[LocalOpsError, IndexedSeq[Byte]] = {
    sha512(password.getBytes("UTF-8")).take(16).asRight[LocalOpsError]
  }

  /** This trims off the first *byte* and the last four checksum bytes from
    * Base58 Ripple encoded things. Suitable for master seed and public key
    *
    * @param rippleB58
    *
    * @return
    */
  def convertBase58Check2hex(rippleB58: String): Either[LocalOpsError, String] = {
    LocalOpsError.wrap(s"Converting Base58Check $rippleB58 to plain hex ") {
      for {
        trimmed <- convertBase58Check2bytes(rippleB58)
        hex = ByteUtils.bytes2hex(trimmed)
      } yield hex
    }
  }

  def convertBase58Check2bytes(rippleB58: String): Either[LocalOpsError, List[Byte]] = {
    LocalOpsError.wrap(s"Converting Base58Check $rippleB58 to Plain Bytes") {
      for {
        bytes <- RippleBase58.decode(rippleB58)
        trimmed = bytes.toList.drop(1).dropRight(4)
      } yield trimmed
    }
  }

  /** Converts Master Seed Hex to Riopple Base58Check encoding */
  def convertSeedHexToB58Check(seedhex: Hex): Either[LocalOpsError, Base58Check] = {
    convertHex2seedB58Check(seedValuePrefix, seedhex)
  }

  /** Converts Public Key Hex to Riopple Base58Check encoding */
  def convertPubKeyHexToB58Check(pubkeyhex: Hex): Either[LocalOpsError, Base58Check] = {
    convertHex2seedB58Check(publicKeyPrefix, pubkeyhex)
  }

  /**
    * Suitable for converted WalletPropose secretKey to master_seed_hex
    *
    * @param masterKeyRFC1751
    *
    * @return Pure format conversion from RFC1751 human words to hex seed
    */
  def convertMasterKey2masterSeedHex(masterKeyRFC1751: String): Either[LocalOpsError, String] = {
    LocalOpsError.wrap("RFC1751 to Master Seed Hex") {
      RFC1751Keys.twelveWordsToHex(masterKeyRFC1751)
    }
  }

  protected def base58Checksum(payload: IndexedSeq[Byte]): Base58Check = {
    val checksum = sha256(sha256(payload.toArray).toArray).take(4)
    val encoded  = RippleBase58.encode(payload ++ checksum)
    Base58Check(encoded)
  }

  /** You will have to add prefix per https://xrpl.org/base58-encodings.html */
  protected def convertHex2seedB58Check(prefix: Hex, hex: Hex): Either[LocalOpsError, Base58Check] = {
    // This is pure conversion, no family stuff. Adds s and checksum
    for {
      header <- ByteUtils.hex2byte(prefix.v)
      bytes  <- ByteUtils.hex2bytes(hex.v)
      payload = header +: bytes
      b58c    = base58Checksum(payload.toIndexedSeq)
    } yield b58c

  }

}

object RippleFormatConverters extends RippleFormatConverters
