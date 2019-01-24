package scala.meta.internal.metals

import scala.meta.AbsolutePath
import scalafix.interfaces.Scalafix
import scala.meta.internal.metals.MetalsEnrichments._
import java.util.Optional
import scalafix.interfaces.ScalafixDiagnostic
import scala.collection.mutable.ListBuffer

class ScalafixProvider(
    workspace: AbsolutePath,
    userConfig: () => UserConfiguration,
    diagnostics: Diagnostics
) {

  private def scalafixConf: AbsolutePath =
    workspace.resolve(userConfig().scalafixConfigPath)

  val scalafix = {
    val scalacOptions = {
      val a = new java.util.ArrayList[String]()
      a.add("-Ywarn-unused-imports")
      a
    }
    Scalafix
      .classloadInstance(this.getClass.getClassLoader)
      .newArguments()
      .withScalacOptions(scalacOptions)
  }

  def didChange(path: AbsolutePath) = {
    val dd = ListBuffer[ScalafixDiagnostic]()
    val errors = scalafix
      .withConfig(Optional.of(scalafixConf.toNIO))
      .withPaths(List(path.toNIO).asJava)
      .withMainCallback(d => dd += d)
      .run()
    diagnostics.onLintErrors(path, dd.toList)
  }

}
