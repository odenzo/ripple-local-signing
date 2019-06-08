package com.odenzo.ripple.localops.impl.utils

import java.math.BigInteger
import scala.annotation.tailrec

import cats._
import cats.data._
import cats.implicits._
import scribe.Logging

import com.odenzo.ripple.localops.impl.utils.caterrors.AppError

// Based on
// https://github.com/ACINQ/bitcoin-lib/blob/master/src/main/scala/fr/acinq/bitcoin/Base58.scala
object RippleBase58 extends Logging {

  val alphabet = "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"

  // char -> value
  val base58Map: Map[Char, BigInt] = alphabet.zipWithIndex.map {
    case (c, indx) => (c, BigInt(indx))
  }.toMap

  /**
    * This should never fail
    *
    * @param input binary data that will be converved to Java Array
    *
    * @return the base-58 representation of input
    */
  def encode(input: IndexedSeq[Byte]): String = {
    if (input.isEmpty) ""
    else {
      val big     = new BigInteger(1, input.toArray)
      val builder = new StringBuilder

      @tailrec
      def encode1(current: BigInteger): Unit = current match {
        case BigInteger.ZERO => ()
        case _ =>
          val res: Array[BigInteger] = current.divideAndRemainder(BigInteger.valueOf(58L))
          val x                      = res(0)
          val remainder              = res(1)
          builder.append(alphabet.charAt(remainder.intValue))
          encode1(x)
      }

      encode1(big)
      input.takeWhile(_ === 0).map(_ => builder.append(alphabet.charAt(0)))
      builder.toString().reverse
    }
  }

  /**
    * This potentially fails if String has char not in the alphabet.
    * Seems a bit wacky overall.
    *
    * @param input base-58 encoded data
    *
    * @return the decoded data
    */
  def decode(input: String): Either[AppError, Iterator[Byte]] = {
    AppError.wrapPure(s"B58 Decoding $input") {

      val zeroes: Iterator[Byte] = input.takeWhile(_ === '1').iterator.map(_ => 0.toByte)
      val trim: String           = input.dropWhile(_ === '1')

      val zeroBI: BigInt = BigInt(0)
      trim match {
        case str if str.isEmpty => zeroes
        case str =>
          val decoded: BigInt = trim.foldLeft(zeroBI)((a, b) => (a * BigInt(58L)) + base58Map(b))
          zeroes ++ decoded.toByteArray.dropWhile(_ === 0) // BigInteger.toByteArray may add a leading 0x00
      }

    }
  }
}
