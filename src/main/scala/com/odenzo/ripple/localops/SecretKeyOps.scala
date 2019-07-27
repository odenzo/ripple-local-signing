package com.odenzo.ripple.localops

import cats._
import cats.data._
import cats.implicits._
import scribe.Logging

import com.odenzo.ripple.localops.impl.{Signer, WalletGenerator}
import com.odenzo.ripple.localops.impl.crypto.RippleFormatConverters
import com.odenzo.ripple.localops.impl.utils.caterrors.AppError

/**
  * Contains functions to pre-process Ripple secret key information into SigningKey objects
  * for more efficent and consistent API code usage.
  * This currently leaks AppError which is OK.
  */
trait SecretKeyOps extends Logging {

  // TODO: Add more Wallet API (maybe only for re-viving an existing one or letting people do their own random

  /**
    * Generates two sets of keys, but doesn't activate them in any way.
    *
    * @return Master and Regular KeyPair based on random seed.
    */
  def generateKeys(keyType: KeyType): Either[AppError, (WalletProposeResult, WalletProposeResult)] = {

    for {
      master  <- WalletGenerator.generateWallet(keyType)
      regular ← WalletGenerator.generateWallet(keyType)
    } yield (master, regular)
  }

  /**
    * Wrapws keypair in SigningKey for optimzed usage. Typically client cache's this.
    *
    * @param master_seed_hex The master seed of an keypair, either master or regular keypair.
    * @param key_type     ED25519 or SECP2561K1
    *
    * @return An opaque structure that should not be relied on for anyway.
    *         Clients should persist keys using the master_seed in an encrypted vault.
    */
  def packSigningKey(master_seed_hex: String, key_type: KeyType): Either[AppError, SigningKey] = {
    Signer.preCalcKeys(master_seed_hex, key_type)
  }

  /**
    * Wrapws keypair in SigningKey for optimzed usage. Typically client cache's this.
    *
    * @param master_seed The master seed of an keypair, either master or regular keypair.
    * @param key_type     ED25519 or SECP2561K1
    *
    * @return An opaque structure that should not be relied on for anyway.
    *         Clients should persist keys using the master_seed in an encrypted vault.
    */
  def packSigningKeyFromB58(master_seed: String, key_type: KeyType): Either[AppError, SigningKey] = {
    RippleFormatConverters
      .convertBase58Check2hex(master_seed)
      .flatMap(packSigningKey(_, key_type))
  }

  /** Pack a key into internal format. Parameters per WalletProposeRs */
  def packSigningKeyFromRFC1751(master_key: String, key_type: KeyType): Either[AppError, SigningKey] = {
    RippleFormatConverters
      .convertMasterKey2masterSeedHex(master_key)
      .flatMap(packSigningKey(_, key_type))
  }

  def precomputeSigningKey(master_seed: String, keyType: KeyType): Either[AppError, SigningKey] = {
    packSigningKeyFromB58(master_seed, keyType)
  }
}

object SecretKeyOps extends SecretKeyOps
