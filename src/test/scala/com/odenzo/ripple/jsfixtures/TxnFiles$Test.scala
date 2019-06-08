package com.odenzo.ripple.jsfixtures

import cats.implicits._
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.{OTestSpec, OTestUtils}

/**
* This looks in the fixture directory for pairs of files matching
  * xxxx-tx.json and xxxxx-binary.json
  *
  * There are a few odditys, leger-full38129.json and ledger-full-40000.json
  * As well as the signerlistset-tx- which that tx, binary and tx-meta-binary.json files
  *
  * These are almost all response things from looking, since I only care about Json => Binary skipping for now.
  */
class TxnFiles$Test extends FunSuite with OTestSpec with OTestUtils {

  /*
  const SignerListSet = {
  tx: require('./fixtures/signerlistset-tx.json'),
  binary: require('./fixtures/signerlistset-tx-binary.json'),
  meta: require('./fixtures/signerlistset-tx-meta-binary.json')
};
const DepositPreauth = {
  tx: require('./fixtures/deposit-preauth-tx.json'),
  binary: require('./fixtures/deposit-preauth-tx-binary.json'),
  meta: require('./fixtures/deposit-preauth-tx-meta-binary.json')
};
const Escrow = {
  create: {
    tx: require('./fixtures/escrow-create-tx.json'),
    binary: require('./fixtures/escrow-create-binary.json')
  },
  finish: {
    tx: require('./fixtures/escrow-finish-tx.json'),
    binary: require('./fixtures/escrow-finish-binary.json'),
    meta: require('./fixtures/escrow-finish-meta-binary.json')
  },
  cancel: {
    tx: require('./fixtures/escrow-cancel-tx.json'),
    binary: require('./fixtures/escrow-cancel-binary.json')
  }
}
const PaymentChannel = {
  create: {
    tx: require('./fixtures/payment-channel-create-tx.json'),
    binary: require('./fixtures/payment-channel-create-binary.json')
  },
  fund: {
    tx: require('./fixtures/payment-channel-fund-tx.json'),
    binary: require('./fixtures/payment-channel-fund-binary.json')
  },
  claim: {
    tx: require('./fixtures/payment-channel-claim-tx.json'),
    binary: require('./fixtures/payment-channel-claim-binary.json')
  }
}

  */
}
