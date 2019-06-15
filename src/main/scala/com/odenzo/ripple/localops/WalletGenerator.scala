package com.odenzo.ripple.localops

import com.odenzo.ripple.localops.crypto.{AccountFamily, RippleFormatConverters}
import com.odenzo.ripple.localops.crypto.core.HashOps
import com.odenzo.ripple.localops.utils.ByteUtils

/** In Progress */
object WalletGenerator {

  def generate(passphrase:String, keyType:String) = {

    val secretKey: Seq[Byte] = HashOps.sha512Half(passphrase.getBytes("UTF-8")) // ??
    val secretKeyHex = ByteUtils.bytes2hex(secretKey)
    

    // val publicKey = derivePublicKey
    val publicKeyBytes: Array[Byte] = Array.emptyByteArray
    RippleFormatConverters.accountpubkey2address(publicKeyBytes)
    
  }
}
