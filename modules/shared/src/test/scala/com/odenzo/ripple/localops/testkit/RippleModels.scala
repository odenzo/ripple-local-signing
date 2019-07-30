package com.odenzo.ripple.localops.testkit

import cats.Show
import cats.implicits._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

/*
  "account_id" : "ra8iBMyU6JrEVLxBBG98sWnmai3fKdTZvd",       AKA: AccountAddress
  "key_type" : "secp256k1",
  "master_key" : "FOLD SAT ORGY PRO LAID FACT TWO UNIT MARY SHOD BID BIND",
  "master_seed" : "sn9tYCjBpqXgHKwJeMT1LC4fdC17d",
  "master_seed_hex" : "B07650EDDF46DE42F9968A1A2557E783",
  "public_key" : "aBPUAJbNXvxP7uiTxmCcCpVgrGjsbJ8f7hQaYPRrdajXNWXuCNLX",
  "public_key_hex" : "02A479B04EDF3DC29EECC89D5F795E32F851C23B402D637F5552E411C354747EB5"


Base58 -- Like Ripple Address , Public Key, Validation Seed
RFC-1751 -- like master_key and validation_key

 Same hacks copied in from old models to make parsing fixtures easier.

 *
 *
 */

/**
  * I use signature to normalize the different types of keys and associated information required in requests
  * Also to mask the values in toString here, if the secret if > 4 since I have hacky tests .
  * But for now, only support the RippleMasterSeed from AccountKeys.
  */
sealed trait RippleSignature

/** Represents a raw Ripple Account Address (sometimes called accountId (e.g. AccountKeys payload) */
final case class AccountAddr(address: Base58) {
  //require(address.v.startsWith("r"), s"Ripple Account Address doesnt start with r :: [$address]")

}

object AccountAddr {
  implicit val encoder: Encoder[AccountAddr] = Encoder.encodeString.contramap[AccountAddr](_.address.v.toString)
  implicit val decoder: Decoder[AccountAddr] = Decoder.decodeString.map(v => AccountAddr(Base58(v)))
  implicit val show: Show[AccountAddr]       = Show.show[AccountAddr](v => v.address.v)

}

case class Base58(v: String) extends AnyVal {}

object Base58 {
  implicit val encoder: Encoder[Base58] = Encoder.encodeString.contramap[Base58](_.v)
  implicit val decoder: Decoder[Base58] = Decoder.decodeString.map(Base58.apply)
}

/** The word based one */
case class RFC1751(v: String) extends AnyVal

object RFC1751 {
  implicit val encoder: Encoder[RFC1751] = Encoder.encodeString.contramap[RFC1751](_.v)
  implicit val decoder: Decoder[RFC1751] = Decoder.decodeString.map(RFC1751.apply)
}

object RippleSignature {

  implicit val encoder: Encoder[RippleSignature] = Encoder.instance[RippleSignature] {
    case d: RippleSeed => d.asJson
    case d: RippleKey  => d.asJson
  }

  def mask(s: String): String = s.zipWithIndex.map(c => if (c._2 > 4 & c._2 % 2 === 1) '*' else c._1).mkString
}

/** Represent a ripple public key. There are ones for accounts, and also ones for validation.
  * Both are repesented as this object for now, and must begin with "n"
  * Account Public Keys start with a
  * Note sure the sematics of this and the SigningPublicKey
  *
  * @param v e.g. "aBPUAJbNXvxP7uiTxmCcCpVgrGjsbJ8f7hQaYPRrdajXNWXuCNLX"
  * */
case class RipplePublicKey(v: Base58) {}

object RipplePublicKey {
  implicit val decode: Decoder[RipplePublicKey] = Decoder.decodeString.map(v => RipplePublicKey(Base58(v)))
  implicit val encode: Encoder[RipplePublicKey] = Encoder.encodeString.contramap(_.v.v)
}

/**
  * Represents a master seed. This is Base58 and starts with "s"
  *
  * @param v seed, aka secret  "sn9tYCjBpqXgHKwJeMT1LC4fdC17d",
  **/
case class RippleSeed(v: Base58) extends RippleSignature

object RippleSeed {
  implicit val decode: Decoder[RippleSeed] = Decoder.decodeString.map(v => RippleSeed(Base58(v)))
  implicit val encode: Encoder[RippleSeed] = Encoder.encodeString.contramap(_.v.v)

}

/** Represents the RFC-1751 work format of master seeds,
  *
  * @param v RFC-1751 form , e.g.  "FOLD SAT ORGY PRO LAID FACT TWO UNIT MARY SHOD BID BIND"
  **/
case class RippleKey(v: RFC1751) extends RippleSignature

object RippleKey {
  implicit val decode: Decoder[RippleKey] = Decoder.decodeString.map(s => RippleKey(RFC1751(s)))
  implicit val encode: Encoder[RippleKey] = Encoder.encodeString.contramap(_.v.v)
}

/** Not used much now, as default KeyType is only non-experimental key */
case class RippleKeyType(v: String) extends AnyVal

object RippleKeyType {
  implicit val decode: Decoder[RippleKeyType] = Decoder.decodeString.map(RippleKeyType(_))
  implicit val encode: Encoder[RippleKeyType] = Encoder.encodeString.contramap(_.v)
}

/**
  * Essentially like RipplePublicKey except for it can be empty string.
  * If there is a failure in signing
  *
  * @param v None is rendered as ""
  */
case class SigningPublicKey(v: Option[RipplePublicKey] = None)

object SigningPublicKey {

  val empty: SigningPublicKey = SigningPublicKey(None)
  implicit val encoder: Encoder[SigningPublicKey] = Encoder.instance[SigningPublicKey] {
    case SigningPublicKey(Some(key)) => key.asJson
    case SigningPublicKey(None)      => Json.fromString("")

  }

  /**
    * Before trying to decode, replace "" with Json.Null which should be pased on None
    */
  implicit val decoder: Decoder[SigningPublicKey] = Decoder[Option[RipplePublicKey]]
    .prepare { ac =>
      ac.withFocus { json =>
        if (json === Json.fromString("")) Json.Null
        else json
      }
    }
    .map(opk => SigningPublicKey(opk))

  def apply(k: RipplePublicKey): SigningPublicKey = SigningPublicKey(Some(k))

  def apply(k: Base58): SigningPublicKey = SigningPublicKey(Some(RipplePublicKey(k)))

}

/**
  *
  *
  * "result" : {
  * "account_id" : "r99mP5QSjNdsEtng26uCnrieTZQe1wNYkf",
  * "key_type" : "secp256k1",
  * "master_key" : "MEND TIED IT NINA AVID SHE ROTH ANTE JUDO CHOU THE OWLY",
  * "master_seed" : "shm5hC3ZoiWUy6GALxajhF5ddXqvC",
  * "master_seed_hex" : "950314B38DAAC9D277F844627B6C3DBA",
  * "public_key" : "aBQk4H5STdAr68s5nY371NRp4VfAwdoF3zsvh3CKLVezPQ64XyZJ",
  * "public_key_hex" : "036F89F2B2E5DC47E4F72B7C33169F071E9F476DAD3D20EF39CA3778BC4508F102"
  * }
  *
  *
  */
case class AccountKeys(
    account_id: AccountAddr,
    key_type: RippleKeyType,
    master_key: RippleKey,
    master_seed: RippleSeed,
    master_seed_hex: String,
    public_key: RipplePublicKey,
    public_key_hex: String
) {

  def address: AccountAddr = account_id

}

/**
  * Once a tx_json is signed there is a TxnSignature which is what this represents. Not used much so far.
  *
  * @param v
  */
case class TxSignature(v: String) extends AnyVal

object TxSignature {
  implicit val decoder: Decoder[TxSignature] = Decoder.decodeString.map(TxSignature(_))
  implicit val encoder: Encoder[TxSignature] = Encoder.encodeString.contramap[TxSignature](_.v)
}

object AccountKeys {
  implicit val encoder: Encoder.AsObject[AccountKeys] = deriveEncoder[AccountKeys]
  implicit val decoder: Decoder[AccountKeys]          = deriveDecoder[AccountKeys]
}

/** These are normally objects, but for potential  error cases keep as Json for now */
case class JsonReqRes(rq: Json, rs: Json)

object JsonReqRes {
  implicit val show: Show[JsonReqRes] = Show.show[JsonReqRes] { rr =>
    s"""
       | rq: ${rr.rq.show}
       | rs: ${rr.rs.show}
     """.stripMargin

  }
  implicit val encoder: Encoder.AsObject[JsonReqRes] = deriveEncoder[JsonReqRes]
  implicit val decoder: Decoder[JsonReqRes]          = deriveDecoder[JsonReqRes]

  def empty = JsonReqRes(Json.Null, Json.Null)
}
