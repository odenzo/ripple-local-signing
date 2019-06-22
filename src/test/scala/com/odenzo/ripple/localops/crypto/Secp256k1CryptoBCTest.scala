package com.odenzo.ripple.localops.crypto

import java.math.BigInteger
import java.security.{KeyFactory, Security}

import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.OTestSpec
import com.odenzo.ripple.localops.crypto.core.{Secp256K1CryptoBC}
import com.odenzo.ripple.localops.utils.ByteUtils


class Secp256k1CryptoBCTest extends FunSuite with OTestSpec with ByteUtils {
  Security.addProvider(new BouncyCastleProvider)

  val fact: KeyFactory = KeyFactory.getInstance("EC", "BC")


  test("About Ripple") {
    val masterSeed       = "F5933E5F60ED0F7A940E995B26F9191E"
    val accountD         = "35778234256876764691539509132472247051158811702107230030471922537290092532408"
    val accountPublicKey = "03AC6788F14F95D87AFF0236A3F671DBF774F24B9E9E94C2B188E9E82DD2F36C21"



    val rippleD    = new BigInteger(accountD)
    val rippleDHex = ByteUtils.bytes2hex(rippleD.toByteArray)
    logger.info(s"RippleD Hex:\n $rippleDHex")
    val rippleKeyPair = Secp256K1CryptoBC.dToKeyPair(rippleD)
    logger.info("Ripple KeyPair: " + rippleKeyPair.getPrivate)
    val pub = Secp256K1CryptoBC.compressPublicKey(rippleKeyPair.getPublic)
      bytes2hex(pub) shouldEqual accountPublicKey
  }



}
