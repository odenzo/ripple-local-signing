
# ripple-local-signing
[![Build Status](https://travis-ci.com/odenzo/ripple-local-signing.svg?branch=master)](https://travis-ci.com/odenzo/ripple-local-signing)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/64c5333412184e23a22590db35f72181)](https://www.codacy.com/app/odenzo/ripple-local-signing?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=odenzo/ripple-local-signing&amp;utm_campaign=Badge_Grade)
[![codecov](https://codecov.io/gh/odenzo/ripple-local-signing/branch/master/graph/badge.svg)](https://codecov.io/gh/odenzo/ripple-local-signing)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


Scala 12.x implementation of secp256k1 and ed25519 transaction signing for Ripple WebSocket API transactions.
Also does verification of transaction signatures.

* Depends on seperate seperate Scala binary serialization library [https://github
.com/odenzo/ripple-binary-codec]

* Depends on BouncyCastle "org.bouncycastle" % "bcprov-jdk15on" % "1.6.2" 

Main use case is just signing and signature verification of transaction Ripple transaction (WebSocket API). Supports 
"fast path" of precalculated the SigningKeys (in internal format) of an account and then signing just to a TxBlob for
 inclusion in SubmitRq.
 



## Usage and Status

This is a library, with com.odenzo.ripple.localops.RippleLocalAPI being the standardized public api.
This is stable and unlikely to change, although additional API may be added.


## Limitations

- Its relatively fast, but the code is not optimized 
- It does not (and probably never will) auto-fill any fields
- It does not (maybe will) check for the presence of empty auto-fill fields (e.g. Fee and Sequence)

### Quality/Testing

* Testing down with fixtures generated on XRPL TestNet for variety of transactions
* Tested with different keys:
    - Master Keys only; secp256k1 and ed25519
    - Master Keys and Regular Keys (all combinations)
    - Using 'secret' for secp256k1 keys
    - Using 'key_type' and 'seed' or 'seed_hex' for all keys






## License TBD

## Build and Publish TBD

+ Will publish somewhere, for now I use sbt publishLocal
