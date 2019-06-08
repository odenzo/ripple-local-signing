package com.odenzo.ripple.localops.models

import cats._
import cats.data._
import cats.implicits._
import io.circe.{Decoder, Encoder}

sealed trait KeyType {
  val txt: String

}

case object SECP256K1 extends KeyType {
  val txt = "secp256k1"
}
case object ED25519 extends KeyType {
  val txt = "ed25519"
}

object KeyType {
  implicit val decoder: Decoder[KeyType] = Decoder[String].emap(s => fromText(s).leftMap(_.error))
  implicit val encoded: Encoder[KeyType] = Encoder[String].contramap[KeyType](kt => kt.txt)
  //implicit val edEncoded: Encoder[KeyType]   = Encoder[String].contramap[KeyType](v => v.txt)
  //implicit val secpEncoded: Encoder[KeyType] = Encoder[String].contramap[KeyType](v => v.txt)

  def fromText(s: String): Either[ResponseError, KeyType] = {
    s.toLowerCase() match {
      case ED25519.txt   => ED25519.asRight
      case SECP256K1.txt => SECP256K1.asRight
      case other         => ResponseError.invalid(s"Invalid Key Type: $other ").asLeft
    }
  }
}
