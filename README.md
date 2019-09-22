
# ripple-local-signing
 [![Download Latest](https://api.bintray.com/packages/odenzooss/maven/ripple-local-signing/images/download.svg)](https://bintray.com/odenzooss/maven/ripple-local-signing/_latestVersion)
[![Build Status](https://travis-ci.com/odenzo/ripple-local-signing.svg?branch=master)](https://travis-ci.com/odenzo/ripple-local-signing)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/64c5333412184e23a22590db35f72181)](https://www.codacy.com/app/odenzo/ripple-local-signing?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=odenzo/ripple-local-signing&amp;utm_campaign=Badge_Grade)
[![codecov](https://codecov.io/gh/odenzo/ripple-local-signing/branch/master/graph/badge.svg)](https://codecov.io/gh/odenzo/ripple-local-signing)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


# Overview
                                                  
A JVM implementation of Ripple (XRPL) local operations:
* Local Signing of Transactions
* Local Multisigning of Transactions, including merging of seperate mult-sign results
* Local Verification of Signing in transactions and signed ledger items
* Local Wallet Propose implementation

All these operate on both  ed25519 and secp256k1 cryptographic key types, and all the variety or Ripple key encodings. 

Drop in replacement for Ripple message processing in `MessageBasedAPI` and lower level operations for more efficient
 processing in `RippleLocalAPI`


## Technical Environment

* Cross Build on Scala 2.12.9 and 2.13.1
* Should be easily usable as a Java library
* Depends on seperate seperate binary serialization library
[https://github.com/odenzo/ripple-binary-codec]

* Depends on BouncyCastle "org.bouncycastle" % "bcprov-jdk15on" % "1.6.2" 




## Quick Start

Published under BinTray, so in SBT: 

```
   // Where version is the value in the badge above, e.g. "0.3.0"
   // Note the badge is the latest version, not necessary the one corresponding to the version matching this README. 
   resolvers in ThisBuild += Resolver.bintrayRepo("odenzooss", "maven"),
   libraryDependencies += "com.odenzo" %% "ripple-local-signing" % version
```

All other needed dependancies will be pulled in.


The basic message api are straightforward :

```scala                        
  val mySignRq:String = """{ "id" = 1 , "tx_json"=...  }"""

  val json   = RippleLocalAPI.parseJsonUNSAFE(mySignRq)
  val signRs =  MessageBasedAPI.sign(json)   
  ```

RippleLocalAPI has more fine grained calls, e.g. to allow just passing in tx_json and getting the tx_blob back for
 submission.


## Status

This is production ready; it doesn't do any auto-filling or extensive validation but if you feed it garbage and it
 happens to make it through, then the submission to real rippled server will flag invalid parameters.

### Bugs/Issues
* Tested underlying functionality pretty extensively, and all *seems* to work. Let me know if an issue and will fix.
* Not really optimized, but majority of time is doing the cryptographic operations. Using the fine grained APIs can
 precompute/convert Ripple secret keys to reduce some time when making multiple calls using the same secret keys.


## License 

Apache 2.0

