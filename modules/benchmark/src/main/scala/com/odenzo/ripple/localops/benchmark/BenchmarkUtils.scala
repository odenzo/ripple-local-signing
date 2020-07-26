package com.odenzo.ripple.localops.benchmark

import java.io.{BufferedWriter, File, FileWriter}
import java.net.URL
import scala.io.{BufferedSource, Source}

import io.circe.Json

import com.odenzo.ripple.localops.LocalOpsError
import com.odenzo.ripple.localops.impl.utils.JsonUtils

object BenchmarkUtils {

  def writeToFile(outFile: String, json: Json): Unit = {
    val file = new File(outFile)
    val bw   = new BufferedWriter(new FileWriter(file))
    val txt  = JsonUtils.droppingNullsPrinter.print(json)
    bw.write(txt)
    bw.close()
    scribe.info(s"Completed with output to ${file.getAbsolutePath}")
  }

  def loadJsonResource(in: String): Either[LocalOpsError, Json] = {
    LocalOpsError.handleM(s"Getting Resource [$in]") {
      val resource: URL          = getClass.getResource(in)
      val source: BufferedSource = Source.fromURL(resource)
      val data: String           = source.getLines().mkString("\n")
      JsonUtils.parseAsJson(data)
    }
  }

  def loadJsonFile(in: String): Either[LocalOpsError, Json] = {
    LocalOpsError.handleM(s"Getting File [$in]") {
      val resource: URL          = getClass.getResource(in)
      val source: BufferedSource = Source.fromFile(in, "UTF-8")
      val data: String           = source.getLines().mkString("\n")
      JsonUtils.parseAsJson(data)
    }
  }
}
