package com.odenzo.ripple.localops

import com.odenzo.ripple.localops.crypto.AccountFamily
import com.odenzo.ripple.localops.crypto.core.HashingOps
import com.odenzo.ripple.localops.utils.ByteUtils

/** In Progress */
object WalletGenerator {

  def generate(passphrase:String, keyType:String) = {

    val secretKey: Seq[Byte] = HashingOps.sha512Half(passphrase.getBytes("UTF-8")) // ??
    val secretKeyHex = ByteUtils.bytes2hex(secretKey)
    

    // val publicKey = derivePublicKey
    val publicKeyBytes: Array[Byte] = Array.emptyByteArray
    AccountFamily.accountpubkey2address(publicKeyBytes)
    
  }
}
