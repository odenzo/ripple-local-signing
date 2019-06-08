# Notes on Cryptography Operations

General notes, ones specific to BouncyCastle and also to Ripple AccountFamily.


# Ripple Account Family

- "Account" is created with WalletPropose, typically without initial password.
- Specify the scheme to use with secp256k1 or ed25199 

## Generating an Account Family

An `Account Family` KeyPair can be used to generate N  `Account` KeyPairs.


### Wallet Propose

Before a Wallet Propose transaction would be used, but can also do algorithmically.

This is the original way Ripple encoded stuff.

Typicaly response (from test-net) is
```json
 {
  "Response": {
      "id": "62d97ceb-9127-443e-a774-ec067ca65b35",
      "result": {
        "account_id": "rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh",
        "key_type": "secp256k1",
        "master_key": "I IRE BOND BOW TRIO LAID SEAT GOAL HEN IBIS IBIS DARE",
        "master_seed": "snoPBrXtMeMyMHUVTgbuqAfg1SUTb",
        "master_seed_hex": "DEDCE9CE67B451D852FD4E846FCDE31C",
        "public_key": "aBQG8RQAzjs1eTKFEAQXr2gS4utcDiEC9wmi7pfUPTi27VCahwgw",
        "public_key_hex": "0330E7FC9D56BB25D6893BA3F317AE5BCF33B3291BD63DB32654A313222F7FD020",
        "warning": "This wallet was generated using a user-supplied passphrase that has low entropy and is vulnerable to brute-force attacks."
      },
      "status": "success",
      "type": "response"
    }
 }
 ```

- `account_id` is the public Ripple Address, this can be derived from the `Account Public Key`
- `key_type` is currency secp256k1 or ed25199
-  `master_key` `master_seed` and `master_seed_hex` are all equivalents to `Account Family` private key
- `Account Family` is also known as `Account Generator`, it is used to create the `Account Public Key` and `Account 
Private Key` (not shown)

- `public_key` and `public_key_hex` are simple encodings

### High Level Algorithm

1. `f(passphrase) => master_seed 

   - Via passphrase:String => UTF-BYTES => SHA512 Hash => Take first 16 bytes
   - Where master_seed is the AccountFamilyPrivateKey.

1. `f(master_key) => master_seed`
    - Master Key if RFC1751 encoded form of master_seed
    - Currently dropping the first two characters of master_key and converting seems to work.
     
2. 
    `f(masterseed, public_key, curve.order) => AccountPrivateKey`

    `f(AccountFamilyPrivateKey, AccountFamilyPublicKey, curve.order) => AccountPrivateKey`
 
   - Via algorithm than uses SHA512 and the order of the elliptic  curve being used. The curve being ed25199 or 
  secp256k1.  *Same algorithm seems to work for both*

1. `f(AccountPrivateKey, curve) => AccountPublicKey`

    - This uses some basic elliptic curve math to calculate. 

1. `f(AccountPublicKey) => AccountAddress`
    - Account Address is `account_id` and is your public ripple address.
    - This address form is actually Ripple Base58 encoded and has a 4 byte checksum on the end.
    - Hashes so cannot go from AccountAddress to AccountPublicKey 


# ECDA with curve secp256k1

- This assumes we have AccountPublicKey and AccountPrivateKey, call this AccountKeyPair.

- Code goes from AccountFamilyKeyPair =>  AccountKeyPair for test data.
- Checking the AccountPublicKey is correct based on tracing Ripple Signing Transactions.

- Subset of SignRs below for illustration:
```json
 {
  "result": {
         "deprecated": "This command has been deprecated and will be removed in a future version of the server. Please migrate to a standalone signing tool.",
         "tx_blob": "1200002280000000240000000161400000002114A0C068400000000000000A7321039BA9FBEEF3CC5AA9CF4C08D4C978FCC445C7AEB951638B49E8CCB411A73A208674473045022100A8361198D1FAA8C62FE77CDD2669FECA95272FAC2C4A08C0BF44E03BB9A9A9B302204214CA2DFB088855FA98AB936A65D4552C027B0D0D3152497B8D63CBCA31D3ED8114632632A78B7DF23B9DD88488D644AB4679F56C9583143C8363862CF094046F82F7676F5EAEF47FFA5EF3F9F1",
         "tx_json": {
           "Account": "rwsECpVPXQu4dcd9H5E4sT6d6Q9jHuhUib",
           "Amount": "555000000",
           "Destination": "raWxAZZLdzfRVAUEcr6j4hxXyHxfSG4M1J",
           "Fee": "10",
           "Flags": 2147483648,
           "Memos": [
           ],
           "Sequence": 1,
           "SigningPubKey": "039BA9FBEEF3CC5AA9CF4C08D4C978FCC445C7AEB951638B49E8CCB411A73A2086",
           "TransactionType": "Payment",
           "TxnSignature": "3045022100A8361198D1FAA8C62FE77CDD2669FECA95272FAC2C4A08C0BF44E03BB9A9A9B302204214CA2DFB088855FA98AB936A65D4552C027B0D0D3152497B8D63CBCA31D3ED",
           "hash": "64B2DFCACF39D8FBC4397777C5CBDF4400B748D8DC7CE3ABFF1660757BE42ED1"
         }
       },
       "status": "success",
       "type": "response"
     }
}     
```

- The SigningPubKey is not fixed for an account, it can me MasterKey or a RegularKey in particular.
- Signing a transaction will result in a different signature each time it is signed. Not sure what is added to algorithm
- Based on that I assume the private key is OK.


- Now I want to emulate the signing of hash field, which is 64-bytes long, an SHA-512 hash I am pretty sure.
- (Check above with my Signing Hasher TBD)
- My understanding is I want to do a ECDSA hash with curve secp256k, but not working.
- Two stumbles, loading my AccountPrivateKey in Bouncy Castle and getting correct signature.

- Have documented the assumed Ripple encoding of Signature and implemented against google search spec.
  Some oddities. Going to try 
  - DER-encoded ASN.1 sequence containing two integer values r and s. This signature format has been specified in ANSI X9.62. This is the format in the first set of data you give (note that signature is a total of 70 bytes)
  - Underlying it the r and s values are wrong, so something is up with the actucal s 
- One possible corner case if if the first byte is 

# AccountPrivateKey (for ECDSA/secp256k1)
- 32-bytes long, this is really just a BigIngter or unsigned int.
- I assume there is no special padding or anything to strip.



8

A private key is just a number modulo the order of the curve.

A public key is the (X,Y) coordinate pair corresponding to that number (the private key) multiplied by the base point (which is a property of the curve used).

If you're talking about public keys: you're almost right. The Y coordinate can indeed be computed from the X coordinate, if you know the sign (given the formula y^2 = x^3 + 7, there are two solutions for Y for every X).

In fact, if you're using a recent version of several wallet clients (bitcoind/bitcoin-qt since 0.6.0 for example), this trick is used. It's called compressed public keys, and it means that when spending a transaction output, the public key stored in the spending script (and thus the block chain) only contains the X coordinate and a marker byte to denote which of both Y coordinates is used. This is slightly slower to validate, but saves space.

In practice, public keys are encoded in the following legal ways:

0x02 + [32-byte X coordinate] (if the Y coordinate is even)
0x03 + [32-byte X coordinate] (if the Y coordinate is odd)
0x04 + [32-byte X coordinate] + [32-byte Y coordinate]
(the two solutions for Y always have different oddness, but as we're talking about a coordinate in a finite field rather than a real number, it does not actually have a 'sign')


## Signing Notes

https://github.com/ripple/rippled/blob/develop/src/ripple/protocol/Sign.h#L64-L83

/** Sign an STObject
    @param st Object to sign
    @param prefix Prefix to insert before serialized object when hashing
    @param type Signing key type used to derive public key
    @param sk Signing secret key
    @param sigField Field in which to store the signature on the object.
    If not specified the value defaults to `sfSignature`.
    @note If a signature already exists, it is overwritten.
*/

- Multisign and sign differ


Seems that we get the data to hash, and prepend the hash type before hashing
  s.add32 (HashPrefix::txMultiSign);
  
  https://github.com/ripple/rippled/blob/develop/src/ripple/protocol/impl/STValidation.cpp



https://bitcoin.stackexchange.com/questions/18858/what-is-the-algorithm-for-generating-a-ripple-address-from-a-ecdsa-public-key
