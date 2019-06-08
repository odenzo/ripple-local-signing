package com.odenzo.ripple.localops.crypto

import java.math.BigInteger
import java.security.{KeyFactory, Security}

import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.OTestSpec
import com.odenzo.ripple.localops.crypto.core.{Secp256K1CryptoBC}
import com.odenzo.ripple.localops.utils.ByteUtils

/** Imports bedevilling me... */
class Secp256k1CryptoBCTest extends FunSuite with OTestSpec {
  Security.addProvider(new BouncyCastleProvider)

  val fact: KeyFactory = KeyFactory.getInstance("EC", "BC")


  test("About Ripple") {
    // And from the SigningOps I got an Account Private Key below that matched the public key
    val masterSeed       = "F5933E5F60ED0F7A940E995B26F9191E"
    val accountD         = "35778234256876764691539509132472247051158811702107230030471922537290092532408"
    val accountPublicKey = "03AC6788F14F95D87AFF0236A3F671DBF774F24B9E9E94C2B188E9E82DD2F36C21"

    val masterSeedBytes                = getOrLog(ByteUtils.hex2Bytes(masterSeed))
    val generator                      = AccountFamily.seed2FamilyGeneratorSecp(masterSeedBytes) // Has its own KeyPair
    val generatorPrivateKeyHex: String = ByteUtils.bytes2hex(generator.privateKey)
    val generatorPublicKeyHex: String  = ByteUtils.bytes2hex(generator.publicKey)
    val accountKeys                    = AccountFamily.familygenerator2accountKeyPair(generator)
    val accountPubHex                  = ByteUtils.bytes2hex(accountKeys.publicKey)

    val rippleD    = new BigInteger(accountD)
    val rippleDHex = ByteUtils.bytes2hex(rippleD.toByteArray)
    logger.info(s"RippleD Hex:\n ${rippleDHex}")
    val rippleKeyPair = Secp256K1CryptoBC.dToKeyPair(rippleD)
    logger.info("Ripple KeyPair: " + rippleKeyPair.getPrivate)
  }



}
