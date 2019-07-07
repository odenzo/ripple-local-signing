package com.odenzo.ripple.localops

import com.odenzo.ripple.localops.crypto.{AccountFamily, RippleFormatConverters}
import com.odenzo.ripple.localops.crypto.core.{ED25519CryptoBC, HashOps}
import com.odenzo.ripple.localops.utils.{ByteUtils, RippleBase58}
import com.odenzo.ripple.localops.utils.caterrors.AppError

/** In Progress */
object WalletGenerator {

  def generateSecpKeys(passphrase: String) = {}

  def generateEdKeys(passphrase: String) = {

    for {
      seedHex <- RippleFormatConverters.convertPassphrase2hex(passphrase)
      // Boo... I will need to checksum etc this.
      //seedB58 ← RippleBase58.encode(ByteUtils.hex2bytes(seedHex))
      kp     ← ED25519CryptoBC.seedHex2keypair(seedHex)
      pubHex ← ED25519CryptoBC.publicKey2Hex(kp.getPublic)
      pubBin ← ByteUtils.hex2bytes(pubHex)
      addr   = RippleFormatConverters.accountpubkey2address(pubBin)

    } yield kp
  }

}
