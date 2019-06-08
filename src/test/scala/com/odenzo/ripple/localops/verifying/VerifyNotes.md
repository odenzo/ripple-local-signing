# Verification Failures


## Public Key

I get the SigningPublicKey and convert to uncompressed KeyPair. The X coordinate matches the public key.
Not sure if there are multiple possible Y values, but I do not thing so.

## Serialization
I re-serialize the tx_json without Signing fields. Essentially that means with TxnSignature

Then I need to add a prefix before hashing -- pretty sure of this.

HashPrefix.transactionID or HashPrefix.transaction or HashPrefix.TransactionSig


## Hashing
Now I do a Ripple SHA512Half and use that as a payload to verify.



## Actual Verification

BouncyCastle ...  seems like "NONEwithECDSA" can be used if I do the SHA512Half ahead of time.
If I send in the serialization then "SHA256withECDAS" should work.

Note: All these are with ripple secp256k1 keys
