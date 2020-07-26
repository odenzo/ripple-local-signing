# Testing Approach

This project started out as disorganized hobby work, so not good unit testing.

Instead:
1. Fixture Based Testing
2. API based testing
3. Some sporadic messy unit testing

Need to add more unit tests for edge cases and error paths, but this program is not 
very complex except the AccountFamily and crypto functions

## Fixture Based Testing

Contained in fixtures package with data generated externally with XRPL Local Server and inquiries on XRPL public
 network.
 
Theory is that for a Ripple transaction, the inputs to Signing and 
MultiSigning are really just the "crypto-keys" and the binary serialization (all and for signing).

So, the Sign and MultiSign fixtures use a few transactions, but try and exercise all key types.

The LedgerTxn fixture is used to get coverage on all the different types of fields being binary serialized.
Its tested here instead of binary-codec because testing is done by verifying the signature, which requires binary
 serialization and correct key handling.
 

The fixture check some lower level output, like TxBlob etc, and also do verification.
