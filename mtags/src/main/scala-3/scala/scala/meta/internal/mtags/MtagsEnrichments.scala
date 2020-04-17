package scala.meta.internal.mtags

import org.eclipse.{lsp4j => l}
import dotty.tools.dotc.util.SourcePosition

object MtagsEnrichments extends CommonMtagsEnrichments {

  def (pos: SourcePosition).toLSP: l.Range = {
    new l.Range(
        new l.Position(pos.startLine, pos.startColumn),
        new l.Position(pos.endLine, pos.endColumn)
    )
  }

}

