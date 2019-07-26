package com.odenzo.ripple.jsfixtures

import scala.collection.mutable

import javax.script.ScriptEngineFactory
import org.scalatest.FunSuite

import com.odenzo.ripple.localops.testkit.OTestSpec

/**
  * From signing-data-encoding-test.js
  */
class TxnSigningTest extends FunSuite with OTestSpec {

  import scala.collection.JavaConverters._
  // Lets try and run Javascript!
  // This used to run ok -- then I threw away the script?
  def javascript(jsFile: String): AnyRef = {
    import java.io.FileReader

    import javax.script.ScriptEngineManager
    val manager                                     = new ScriptEngineManager()
    val factories: mutable.Seq[ScriptEngineFactory] = manager.getEngineFactories.asScala
    factories.foreach { v ⇒
      logger.info(s"Factory: ${v.getEngineName} ${v.getEngineVersion} ${v.getLanguageName} ${v.getLanguageVersion}")
    }
    val engine      = manager.getEngineByName("nashorn")
    val res: AnyRef = engine.eval(new FileReader(jsFile))
    res
  }

}
