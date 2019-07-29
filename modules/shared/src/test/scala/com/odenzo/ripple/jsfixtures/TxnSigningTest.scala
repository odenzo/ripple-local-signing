package com.odenzo.ripple.jsfixtures

import java.util

import javax.script.ScriptEngineFactory

import com.odenzo.ripple.localops.testkit.OTestSpec

/**
  * From signing-data-encoding-test.js
  */
class TxnSigningTest extends OTestSpec {

  import scala.jdk.CollectionConverters._
  // Lets try and run Javascript!
  // This used to run ok -- then I threw away the script?
  def javascript(jsFile: String): AnyRef = {
    import java.io.FileReader

    import javax.script.ScriptEngineManager
    val manager                                   = new ScriptEngineManager()
    val factories: util.List[ScriptEngineFactory] = manager.getEngineFactories

    factories.asScala.foreach { v =>
      logger.info(s"Factory: ${v.getEngineName} ${v.getEngineVersion} ${v.getLanguageName} ${v.getLanguageVersion}")
    }
    val engine      = manager.getEngineByName("nashorn")
    val res: AnyRef = engine.eval(new FileReader(jsFile))
    res
  }

}
