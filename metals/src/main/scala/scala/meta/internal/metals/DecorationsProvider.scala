package scala.meta.internal.metals

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.mtags.Semanticdbs
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.Range
import scala.meta.io.AbsolutePath
import scala.meta.internal.{semanticdb => s}

final class DecorationsProvider(
    semanticdbs: Semanticdbs,
    workspace: AbsolutePath
) {

  def decorations(params: DecorationParams): DecorationResult = {
    val source = params.td.getUri.toAbsolutePath
    (for {
      sdb <- semanticdbs.textDocument(source).documentIncludingStale
    } yield {
      val ss = sdb.synthetics.collect {
        case s.Synthetic(Some(range), t @ s.ApplyTree(_: s.OriginalTree, _)) =>
          Decoration(range.toLSP, SemanticdbTreePrinter.printTree(t))
        case s.Synthetic(
            Some(range),
            t @ s.TypeApplyTree(_: s.OriginalTree, _)
            ) =>
          Decoration(range.toLSP, SemanticdbTreePrinter.printTree(t))
      }
      scribe.info(sdb.synthetics.mkString("\n"))
      DecorationResult(ss.toArray)
    }).getOrElse(DecorationResult(Array.empty))
  }

}

final case class DecorationParams(td: TextDocumentIdentifier)

final case class DecorationResult(decorations: Array[Decoration])
final case class Decoration(range: Range, text: String)
