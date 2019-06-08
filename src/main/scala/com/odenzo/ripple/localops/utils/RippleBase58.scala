package com.odenzo.ripple.localops.utils

import java.math.BigInteger
import scala.annotation.tailrec

import com.typesafe.scalalogging.StrictLogging

// Based on
// https://github.com/ACINQ/bitcoin-lib/blob/master/src/main/scala/fr/acinq/bitcoin/Base58.scala
 object RBase58 extends StrictLogging {


  

  val alphabet = "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"

  // char -> value
  val base28Map: Map[Char, Int] = alphabet.zipWithIndex.toMap

  /**
    * This should never fail
    * @param input binary data
    *
    * @return the base-58 representation of input
    */
  def encode(input: Seq[Byte]): String = {
    if (input.isEmpty) ""
    else {
      val big     = new BigInteger(1, input.toArray)
      val builder = new StringBuilder

      @tailrec
      def encode1(current: BigInteger): Unit = current match {
        case BigInteger.ZERO => ()
        case _ =>
          val Array(x, remainder) = current.divideAndRemainder(BigInteger.valueOf(58L))
          builder.append(alphabet.charAt(remainder.intValue))
          encode1(x)
      }

      encode1(big)
      input.takeWhile(_ == 0).map(_ => builder.append(alphabet.charAt(0)))
      builder.toString().reverse
    }
  }

  /**
    * This potentially fails if String has char not in the alphabet.
    * @param input base-58 encoded data
    *
    * @return the decoded data
    */
  def decode(input: String): Array[Byte] = {
    val zeroes = input.takeWhile(_ == '1').map(_ => 0: Byte).toArray
    val trim   = input.dropWhile(_ == '1').toList
    val decoded = trim
      .foldLeft(BigInteger.ZERO)(
        (a, b) =>
          a.multiply(BigInteger.valueOf(58L))
            .add(BigInteger.valueOf(base28Map(b).toLong))
      )
    if (trim.isEmpty) zeroes
    else
      zeroes ++ decoded.toByteArray
        .dropWhile(_ == 0) // BigInteger.toByteArray may add a leading 0x00
  }


  /** This handles Base58 Keys, like Seed and Public Key.
    * It does the normal decoding and drops the first marker byte and the last four checksum bytes (?)
    *
    * @param b58
    */
  def base58Key2bytesTrimmed(b58: String): Array[Byte] = {
     base58Key2bytes(b58).drop(1).dropRight(4)
  }

  def base58Key2bytes(b28: String): Array[Byte] = {
    RBase58.decode(b28)
  }

  def base58ToHex(b28: String): String = {
    ByteUtils.bytes2hex(base58Key2bytes(b28))
  }

}
