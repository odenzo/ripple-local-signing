package com.odenzo.ripple.localops.benchmark

import java.util.concurrent.TimeUnit
import scala.util.Random

import io.circe.syntax._
import io.circe.{Decoder, Json, JsonObject}

import cats._
import cats.data._
import cats.implicits._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import com.odenzo.ripple.bincodec.RippleCodecAPI
import com.odenzo.ripple.localops.impl.utils.JsonUtils
import com.odenzo.ripple.localops.{LocalOpsError, MessageBasedAPI}

/**
  * Basic benchmarkuing, this is not really a microbenchmark but we abuse it.
  * Benchmark to bincodec lib calls and also the full signing to get an idea of % spent in BinCodec
  * The following command will run the benchmarks with reasonable settings:
  *       benchmark/jmh:run  -i 10 -wi 10 -f1 -t1  BenchmarkSignRq
  *
  */
class ProfileSignRq {

  case class SignRqData(v: Vector[Json])
  case class TxJsonData(v: Vector[Json])

  val (signs, txs): (SignRqData, TxJsonData) = {
    for {
      json    <- BenchmarkUtils.loadJsonResource("/sign_benchmarks_data.json")
      rqs     <- JsonUtils.decode(json, Decoder[List[JsonObject]])
      txjsons <- rqs.traverse(o => o.apply("tx_json").toRight(LocalOpsError("Problem Parsing Rq", o.asJson)))
      rqJson = rqs.map(_.asJson)
    } yield (SignRqData(rqJson.toVector), TxJsonData(txjsons.toVector))
  } match {
    case Left(err) => scribe.error(err.show); throw err
    case Right(d)  => d
  }

  def linearize = (1 to 100).toList.it
  def randomSign(): Json = {
    val signIndexRandom    = Random.nextInt(signs.v.length)
    val json: Option[Json] = signs.v.get(signIndexRandom)
    json.getOrElse(Json.Null)
  }
  def randomTx(): Json = {
    val randomIndex        = Random.nextInt(txs.v.length)
    val json: Option[Json] = txs.v.get(randomIndex)
    json.getOrElse(Json.Null)
  }

//   @Fork(value = 1, jvmArgsAppend = Array("-XX:+UnlockCommercialFeatures",
//    "-XX:+FlightRecorder", "-XX:+UnlockDiagnosticVMOptions", "-XX:+DebugNonSafepoints",
//    "-XX:FlightRecorderOptions=defaultrecording=true,dumponexit=true,dumponexitpath=/tmp/mapOnFunctionsAndList.jfr"))

  def serialize(bh: Blackhole): Unit = {
    val rs = RippleCodecAPI.signingTxBlob(randomTx()).leftMap(LocalOpsError.wrapBinaryCodecError)
    bh.consume(rs)
  }

  def sign(bh: Blackhole): Unit = {
    val rs = MessageBasedAPI.sign(randomSign())
    bh.consume(rs)
  }

}
