package scala.meta.languageserver

import org.openjdk.jmh.annotations._
import org.langmeta.inputs.Input
import ctags.Ctags

class CtagsBenchmark {

  @Benchmark
  def scalaCtagsIndex = Ctags.index("test.scala", """
  |object a {
  |  val x = 2
  |}
  """.stripMargin)

}
