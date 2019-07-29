package com.odenzo.ripple.localops.impl

import java.security.SecureRandom

import cats._
import cats.data._
import cats.implicits._
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import scribe.Logging

import com.odenzo.ripple.localops.impl.crypto.core.{ED25519CryptoBC, Secp256K1CryptoBC}
import com.odenzo.ripple.localops.impl.crypto.{AccountFamily, RFC1751Keys, RippleFormatConverters}
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError
import com.odenzo.ripple.localops.impl.utils.{ByteUtils, Hex}
import com.odenzo.ripple.localops.{ED25519, KeyType, SECP256K1, WalletProposeResult}

/** In Progress */
object WalletGenerator extends Logging with ByteUtils {

  /** This is the recommended method. It will generate an ed25519 set of keys.
    * It will not initialize the account (no initial deposit).
    * All the rest of the methods are for compatability for existing legacy use cases.
    * Note: ed25519 keys do not work with Payment Channels (yet?) according to Ripple
    *
    */
  def generateWallet(keytype: KeyType): Either[AppError, WalletProposeResult] = {
    generateSeed().flatMap { seed =>
      keytype match {
        case ED25519   => generateEdKeys(seed)
        case SECP256K1 => generateSecpKeys(seed)
      }
    }
  }

  /** Generates a wallet using Java SecureRandom.  */
  def generateSeed(): Either[AppError, List[Byte]] = {
    val ranBytes = new Array[Byte](16)
    SecureRandom.getInstanceStrong.nextBytes(ranBytes)
    ranBytes.toList.asRight[AppError]
  }

  /**
    * Please use the specific generateXXX whenever possible. Ideally generate from seed_hex
    *
    * @param someSeed
    */
  def generateSeedBySniffing(someSeed: String): Either[AppError, List[Byte]] = {
    // Try and sniff the seed type and delegate, most specific to least with fallback
    // Order is important, ie valid SecretKey cannot be any other thing
    // Valid Hex cannot be valid B58Check. Also need to negative test all those routines.
    // TODO: Seems  SeedB58 is not returning error on all failures (not checking prefix and checksum just chopping)
    generateSeedFromSecretKey(someSeed)
      .recoverWith { case e => generateSeedFromHex(someSeed) }
      .recoverWith { case e => generateSeedFromSeedB58(someSeed) }
      .recoverWith { case e => generateSeedFromPassword(someSeed) }

  }

  def generateSeedFromSecretKey(wordsRFC1751: String): Either[AppError, List[Byte]] = {
    for {
      seedHex <- RippleFormatConverters.convertMasterKey2masterSeedHex(wordsRFC1751)
      bytes   <- hex2bytes(seedHex)
    } yield bytes
  }

  /**
    * Thius just converts to bytes :-). Preferred method
    *
    * @param hex 16 bytes hex
    */
  def generateSeedFromHex(hex: String): Either[AppError, List[Byte]] = hex2bytes(hex)

  def generateSeedFromSeedB58(b58Check: String): Either[AppError, List[Byte]] = {
    RippleFormatConverters.convertBase58Check2bytes(b58Check)
  }

  def generateSeedFromPassword(password: String): Either[AppError, List[Byte]] = {
    RippleFormatConverters.convertPassword2bytes(password)
  }

  /**
    * Canonical generator given the "random" seed
    *
    * @param bytes This requires exactly 16 bytes
    */
  def generateEdKeys(bytes: List[Byte]): Either[AppError, WalletProposeResult] = {
    val kp: AsymmetricCipherKeyPair = ED25519CryptoBC.generateKeyPairFromBytes(bytes)
    for {
      pubHex <- ED25519CryptoBC.publicKey2Hex(kp.getPublic)
      pubB58 <- RippleFormatConverters.convertPubKeyHexToB58Check(Hex(pubHex))
      pubBin <- ByteUtils.hex2bytes(pubHex)
      seedHex = bytes2hex(bytes)
      seedB58 <- RippleFormatConverters.convertSeedHexToB58Check(Hex(seedHex))

      addr <- RippleFormatConverters.accountpubkey2address(pubBin)
      masterKey = RFC1751Keys.bytesToEnglish(bytes.toArray)
    } yield
      WalletProposeResult(
        account_id = addr.v,
        key_type = ED25519,
        master_key = masterKey,
        master_seed = seedB58.v,
        master_seed_hex = seedHex,
        public_key = pubB58.v,
        public_key_hex = pubHex
      )
  }

  def generateSecpKeys(bytes: List[Byte]): Either[AppError, WalletProposeResult] = {

    for {
      kp <- AccountFamily.rebuildAccountKeyPairFromSeed(bytes)
      pubBin = Secp256K1CryptoBC.compressPublicKey(kp.getPublic)
      pubHex = ByteUtils.bytes2hex(pubBin)
      pubB58 <- RippleFormatConverters.convertPubKeyHexToB58Check(Hex(pubHex))

      seedHex = bytes2hex(bytes)
      seedB58 <- RippleFormatConverters.convertSeedHexToB58Check(Hex(seedHex))

      addr <- RippleFormatConverters.accountpubkey2address(pubBin.toIndexedSeq)
      masterKey = RFC1751Keys.bytesToEnglish(bytes.toArray)
    } yield
      WalletProposeResult(
        account_id = addr.v,
        key_type = SECP256K1,
        master_key = masterKey,
        master_seed = seedB58.v,
        master_seed_hex = seedHex,
        public_key = pubB58.v,
        public_key_hex = pubHex
      )
  }

}
