package com.odenzo.ripple.localops

import java.security.{KeyPair, SecureRandom}

import cats._
import cats.data._
import cats.implicits._
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import scribe.Logging

import com.odenzo.ripple.localops.crypto.{AccountFamily, RFC1751Keys, RippleFormatConverters}
import com.odenzo.ripple.localops.crypto.core.{ED25519CryptoBC, HashOps, Secp256K1CryptoBC}
import com.odenzo.ripple.localops.utils.{ByteUtils, Hex, RippleBase58}
import com.odenzo.ripple.localops.utils.caterrors.AppError

/** In Progress */
object WalletGenerator extends Logging with ByteUtils {

  /** Generates a wallet using Java SecureRandom */
  def generateSeed(): Either[AppError, List[Byte]] = {
    val ranBytes = new Array[Byte](16)
    logger.info(s"Random Bytes After Secure: $ranBytes")
    SecureRandom.getInstanceStrong.nextBytes(ranBytes)
    logger.info(s"Random Bytes After Secure: $ranBytes")
    ranBytes.toList.asRight[AppError]
  }

  /**
    *  Please use the specific generateXXX whenever possible. Ideally generate from seed_hex
    * @param someSeed
    */
  def generateSeedBySniffing(someSeed: String): Either[AppError, List[Byte]] = {
    // Try and sniff the seed type and delegate, most specific to least with fallback
    // Order is important, ie valid SecretKey cannot be any other thing
    // Valid Hex cannot be valid B58Check. Also need to negative test all those routines.
    // TODO: Seems  SeedB58 is not returning error on all failures (not checking prefix and checksum just chopping)
    generateSeedFromSecretKey(someSeed)
      .recoverWith{ case e ⇒ generateSeedFromHex(someSeed) }
      .recoverWith { case e ⇒ generateSeedFromSeedB58(someSeed) }
      .recoverWith { case e ⇒ generateSeedFromPassword(someSeed) }

  }

  def generateSeedFromSecretKey(wordsRFC1751: String): Either[AppError, List[Byte]] = {
    for {
      seedHex <- RippleFormatConverters.convertMasterKey2masterSeedHex(wordsRFC1751)
      bytes   ← hex2bytes(seedHex)
    } yield bytes
  }

  /**
    *   Thius just converts to bytes :-). Preferred method
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
    * @param bytes This requires exactly 16 bytes
    */
  def generateEdKeys(bytes: List[Byte]): Either[AppError, WalletProposeResult] = {
    val kp: AsymmetricCipherKeyPair = ED25519CryptoBC.generateKeyPairFromBytes(bytes.toArray)
    for {
      pubHex  ← ED25519CryptoBC.publicKey2Hex(kp.getPublic)
      pubB58  ← RippleFormatConverters.convertPubKeyHexToB58Check(Hex(pubHex))
      pubBin  ← ByteUtils.hex2bytes(pubHex)
      seedHex = bytes2hex(bytes)
      seedB58 ← RippleFormatConverters.convertSeedHexToB58Check(Hex(seedHex))

      addr      <- RippleFormatConverters.accountpubkey2address(pubBin)
      masterKey = RFC1751Keys.bytesToEnglish(bytes.toArray)
    } yield
      WalletProposeResult(
        account_id = addr.v,
        key_type = "ed25519",
        master_key = masterKey,
        master_seed = seedB58.v,
        master_seed_hex = seedHex,
        public_key = pubB58.v,
        public_key_hex = pubHex
      )
  }

  def generateSecpKeys(bytes: List[Byte]): Either[AppError, WalletProposeResult] = {

    for {
      kp     ← AccountFamily.rebuildAccountKeyPairFromSeed(bytes)
      pubBin = Secp256K1CryptoBC.compressPublicKey(kp.getPublic)
      pubHex = ByteUtils.bytes2hex(pubBin)
      pubB58 ← RippleFormatConverters.convertPubKeyHexToB58Check(Hex(pubHex))

      seedHex = bytes2hex(bytes)
      seedB58 ← RippleFormatConverters.convertSeedHexToB58Check(Hex(seedHex))

      addr      <- RippleFormatConverters.accountpubkey2address(pubBin)
      masterKey = RFC1751Keys.bytesToEnglish(bytes.toArray)
    } yield
      WalletProposeResult(
        account_id = addr.v,
        key_type = "secp256k1",
        master_key = masterKey,
        master_seed = seedB58.v,
        master_seed_hex = seedHex,
        public_key = pubB58.v,
        public_key_hex = pubHex
      )
  }

}
