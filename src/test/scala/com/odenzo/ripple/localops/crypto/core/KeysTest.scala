package com.odenzo.ripple.localops.crypto.core

import java.security.{KeyPair, PublicKey}

import org.scalatest.FunSuite

import com.odenzo.ripple.localops.OTestSpec
import com.odenzo.ripple.localops.crypto.AccountFamily.AccountKeyPair
import com.odenzo.ripple.localops.crypto.{AccountFamily, DERSignature, core}
import com.odenzo.ripple.localops.utils.{ByteUtils, RBase58}

class KeysTest extends FunSuite with OTestSpec {

  test("All") {
    // From SigningRq
    val kAddress = "r4vNECFxhD7JPGP5isoPfsyxCagEL3z4dX"
    val kSecret  = "safjcEdiCAB6Ttynbyr5bRnXH7U7i"

    // From SigningRs
    val kSigningPubKey = "03AC6788F14F95D87AFF0236A3F671DBF774F24B9E9E94C2B188E9E82DD2F36C21"
    val kHash          = "B64CB9EA6CF4911E55D8C3EB6B70818834D448566595F5E0A795A3EE7B47A7C8"
    val kTxnsig =
      "3045022100E7B6342C8C76860C84EF024358DEEBF2F0985A9918104C947C3FFA93F681B878022023FACC310744AAFD76B373D6416A51476585B8682B1308F1280016C330682333"

    val kTxBlob =
      "1200002280000000240000000261400000002123E30068400000000000000A732103AC6788F14F95D87AFF0236A3F671DBF774F24B9E9E94C2B188E9E82DD2F36C2174473045022100E7B6342C8C76860C84EF024358DEEBF2F0985A9918104C947C3FFA93F681B878022023FACC310744AAFD76B373D6416A51476585B8682B1308F1280016C3306823338114F06BC01B3C9CD12A9D21016E1298DE03CC7B18178314BDB3DAAA9AF869D285BC895D381AC92C64F2E0EEF9F1"

    // From the Wallet Propose for  matching account address
    val kAccountId     = "r4vNECFxhD7JPGP5isoPfsyxCagEL3z4dX"
    val kKeyType       = "secp256k1"
    val kMasterKey     = "ION RIME AHEM MANY GRUB AJAR EDDY FERN BILL MOOD TENT WHAM"
    val kMasterSeed    = "safjcEdiCAB6Ttynbyr5bRnXH7U7i" // Seed 2 Generator
    val kMasterSeedHex = "F5933E5F60ED0F7A940E995B26F9191E"
    val kPublicKey     = "aBRDBppfdPyu4cFwTZCUqgeaEkszcFPfhjaydVfbDpuepWoEuuis"
    val kPublicKeyHex  = "03AC6788F14F95D87AFF0236A3F671DBF774F24B9E9E94C2B188E9E82DD2F36C21"

    // kTxnSig is in DER or ECDASignature form.
    // Doubt this can be changed at all
    val derSig: DERSignature     = getOrLog(DERSignature.fromHex(kTxnsig))
    val derSigBytes: Array[Byte] = getOrLog(derSig.toByteList).toArray

    // Encode the public key the hard way
    val seed                        = getOrLog(ByteUtils.hex2Bytes(kMasterSeedHex))
    val accountKeys                   = AccountFamily.rebuildAccountKeyPairFromSeedHex(kMasterSeedHex)
//
//
//    val derivedPublicKey: PublicKey = Secp256K1CryptoBC.decompressPublicKey(accountKeys.publicKey)
//    val accountKeyPair: KeyPair     = Secp256K1CryptoBC.dToKeyPair(BigInt(1, accountKeys.privateKey).bigInteger)
//
//    val prefix: Array[Byte] = getOrLog(RBase58.decode(kAccountId))
//    val prefixHex: String   = ByteUtils.bytes2hex(prefix)
//    logger.info(s"Account Bytes with Prefix: $prefixHex")
//
//    val address: String = AccountFamily.accountpubkey2address(accountKeys.publicKey)
//    address shouldEqual kAccountId
//
//    val msgPublicKeyBytes: List[Byte] = getOrLog(ByteUtils.hex2Bytes(kPublicKeyHex))
//
//    val msgPublicKey: PublicKey = Secp256K1CryptoBC.decompressPublicKey(msgPublicKeyBytes.toArray)
//
//    derivedPublicKey shouldEqual accountKeyPair.getPublic
//    msgPublicKey shouldEqual derivedPublicKey
  }
}
