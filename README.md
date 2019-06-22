
# ripple-local-signing

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


Scala 12.x implementation of secp256k1 and ed25519 transaction signing for Ripple WebSocket API transactions.
Also does verification of transaction signatures.

* Depends on seperate seperate Scala binary serialization library 

* Depends on BouncyCastle "org.bouncycastle" % "bcprov-jdk15on" % "1.6.2" 

Main use case is just signing and signature verification of transaction Ripple transaction (WebSocket API).



## Usage and Status

This is a library, with com.odenzo.ripple.localops.RippleLocalAPI being the standardized public api.
This is stable and unlikely to change, although additional API may be added.


## Limitations

### Supports only seed and key_type

Instead of supporting every type of signing secret parameters   seed and key_type 
fields must be used. key_type can be ed25519 or secp256k1.   seed has to be master_seed, Base58 (sXXXXX) style.

There are utilities buried a bit deeply now for converting different "password" types to Base58Seed,
e.g. The words (RFC1751) and master_seed_hex 


### Quality/Testing

* Test fixtures for a variety of WalletPropose Requests and Responses
* Signing and Verification just tested on Payment tx (limited set, only XRP even

* Tested with generated accounts on TestNet, but only with master keys, have set RegularKey and tested those yet.
  Should be OK, as same logic and there is no cross-check to make sure the keys match the address (RegularKeys dont)





## License TBD

## Build and Publish TBD

+ Will publish somewhere, for now I use sbt publishLocal
