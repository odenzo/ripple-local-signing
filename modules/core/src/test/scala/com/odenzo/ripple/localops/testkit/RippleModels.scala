package com.odenzo.ripple.localops.testkit

import cats.Show
import cats.implicits._
import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

sealed trait RippleSignature

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

case class TxSignature(v: String) extends AnyVal

object TxSignature {
  implicit val decoder: Decoder[TxSignature] = Decoder.decodeString.map(TxSignature(_))
  implicit val encoder: Encoder[TxSignature] = Encoder.encodeString.contramap[TxSignature](_.v)
}

object AccountKeys {
  implicit val encoder: Encoder.AsObject[AccountKeys] = deriveEncoder[AccountKeys]
  implicit val decoder: Decoder[AccountKeys]          = deriveDecoder[AccountKeys]
}

case class JsonReqRes(rq: Json, rs: Json)

object JsonReqRes {
  implicit val show: Show[JsonReqRes] = Show.show[JsonReqRes](rr => rr.asJson.spaces4)

  implicit val encoder: Encoder.AsObject[JsonReqRes] = deriveEncoder[JsonReqRes]
  implicit val decoder: Decoder[JsonReqRes]          = deriveDecoder[JsonReqRes]

  def apply(rq: JsonObject, rs: JsonObject): JsonReqRes = JsonReqRes(rq.asJson, rs.asJson)

  def empty = JsonReqRes(Json.Null, Json.Null)
}

// Always tracing these with master key only.
case class MultiSignTrace(account: AccountKeys, signers: List[AccountKeys], signFors: List[JsonReqRes])

case class FKP(master: AccountKeys, regular: Option[AccountKeys])

object MultiSignTrace {

  import io.circe.generic.semiauto._

  implicit val decoder: Decoder[MultiSignTrace] = deriveDecoder[MultiSignTrace]
}

object FKP {
  implicit val decoder: Decoder[FKP] = deriveDecoder[FKP]
}

case class Signers(signers: List[Signer])
object Signers {
  // Signers is an array of objects with each object having a signer

  implicit val decoder: Decoder[Signers] = Decoder[List[JsonObject]].emap { listobj: List[JsonObject] =>
    val signers: Result[List[Signer]] = listobj.flatMap { obj =>
      obj.apply("Signer").map(json => json.as[Signer])
    }.sequence
    val res: Either[String, Signers] = signers.bimap(e => e.message, v => Signers(v))
    res
  }

}
case class Signer(account: AccountAddr, signingPubKey: SigningPublicKey, txnSignature: TxSignature)

object Signer {
  implicit val dec: Decoder[Signer] = Decoder.forProduct3("Account", "SigningPubKey", "TxnSignature")(Signer.apply)
  implicit val enc: Encoder.AsObject[Signer] =
    Encoder.forProduct3("Account", "SigningPubKey", "TxnSignature")(u => (u.account, u.signingPubKey, u.txnSignature))
}
