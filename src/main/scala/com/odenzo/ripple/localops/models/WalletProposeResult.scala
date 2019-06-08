package com.odenzo.ripple.localops.models

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

// This is currently public, but better to return as JsonObject IMHO.
case class WalletProposeResult(
    account_id: String,
    key_type: KeyType,
    master_key: String,
    master_seed: String,
    master_seed_hex: String,
    public_key: String,
    public_key_hex: String
)

object WalletProposeResult {

  implicit val encoder: Encoder.AsObject[WalletProposeResult] = deriveEncoder[WalletProposeResult]
  implicit val decoder: Decoder[WalletProposeResult]          = deriveDecoder[WalletProposeResult]

}
