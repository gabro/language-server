package scala.meta.internal.metals

import scala.meta._
import org.eclipse.{lsp4j => l}
import MetalsEnrichments._
import scala.collection.concurrent.TrieMap

/**
  *  Retrieves all the symbols defined in a document
  *
  *  If the document doesn't parse, we fall back to the latest
  *  known snapshot of the document, if present
  *
  *  To avoid the snapshots to grow undefinitely, we only store
  *  the ones for the most recently requested documents
  */
class DocumentSymbolProvider[A: DocumentSymbolBuilder](buffers: Buffers) {

  private val snapshots: TrieMap[AbsolutePath, List[A]] =
    TrieMap.empty

  private def empty: List[A] = Nil

  def documentSymbols(
      uri: String
  ): List[A] = {
    val path = uri.toAbsolutePath
    path
      .toInputFromBuffers(buffers)
      .parse[Source]
      .toOption
      .map { source =>
        val result = new SymbolTraverser[A](uri).apply(source)
        snapshots.put(path, result)
        result
      }
      .orElse(snapshots.get(path))
      .getOrElse(empty)
  }

  def discardSnapshot(path: AbsolutePath): Unit =
    snapshots.remove(path)

  private class SymbolTraverser[A](uri: String)(implicit documentSymbolBuilder: DocumentSymbolBuilder[A]) {

    def apply(tree: Tree): List[A] = {
      traverser.apply(tree)
      builder.result()
    }

    private val builder = List.newBuilder[A]

    val traverser = new Traverser {
      var currentRoot: Option[Tree] = None
      override def apply(currentNode: Tree): Unit = {
        def continue(withNewRoot: Boolean = false): Unit = {
          val oldRoot = currentRoot
          if (withNewRoot) currentRoot = Some(currentNode)
          super.apply(currentNode)
          currentRoot = oldRoot
        }

        def addName(name: String): Unit = {
          builder += documentSymbolBuilder(
            name,
            symbolKind(currentNode),
            currentNode.pos.toLSP,
            uri
          )
        }

        def addNode(): Unit = names(currentNode).foreach(addName)

        currentNode match {
          // we need to go deeper
          case _: Source | _: Template => continue()
          // add package, but don't set it as a new root
          case _: Pkg =>
            addNode()
            continue()
          // terminal nodes: add them, but don't go inside
          case _: Defn.Def | _: Defn.Val | _: Defn.Var => addNode()
          case _: Decl.Def | _: Decl.Val | _: Decl.Var => addNode()
          // all other (named) types and terms can contain more nodes
          case t if t.is[Member.Type] || t.is[Member.Term] =>
            addNode()
            continue(withNewRoot = true)
          case _ => ()
        }
      }
    }

  }

  /** All names within the node.
    *  - if it's a package, it will have its qualified name: `package foo.bar.buh`
    *  - if it's a val/var, it may contain several names in the pattern: `val (x, y, z) = ...`
    *  - for everything else it's just its normal name (if it has one)
    */
  private def names(tree: Tree): List[String] = tree match {
    case t: Pkg => qualifiedName(t).toList
    case t: Defn.Val => patternNames(t.pats)
    case t: Decl.Val => patternNames(t.pats)
    case t: Defn.Var => patternNames(t.pats)
    case t: Decl.Var => patternNames(t.pats)
    case t: Member => List(t.name.value)
    case _ => Nil
  }

  private def patternNames(pats: List[Pat]): List[String] =
    pats.flatMap { _.collect { case Pat.Var(name) => name.value } }

  private def qualifiedName(tree: Tree): Option[String] = tree match {
    case Term.Name(name) => Some(name)
    case Term.Select(qual, name) =>
      qualifiedName(qual).map { prefix =>
        s"$prefix.$name"
      }
    case Pkg(sel: Term.Select, _) => qualifiedName(sel)
    case m: Member => Some(m.name.value)
    case _ => None
  }

  private def isFunction(tree: Tree): Boolean = {
    val tpeOpt: Option[Type] = tree match {
      case d: Decl.Val => Some(d.decltpe)
      case d: Decl.Var => Some(d.decltpe)
      case d: Defn.Val => d.decltpe
      case d: Defn.Var => d.decltpe
      case _ => None
    }
    tpeOpt.filter(_.is[Type.Function]).nonEmpty
  }

  private def symbolKind(tree: Tree): l.SymbolKind = tree match {
    case f if isFunction(f) => l.SymbolKind.Function
    case _: Decl.Var | _: Defn.Var => l.SymbolKind.Variable
    case _: Decl.Val | _: Defn.Val => l.SymbolKind.Constant
    case _: Decl.Def | _: Defn.Def => l.SymbolKind.Method
    case _: Decl.Type | _: Defn.Type => l.SymbolKind.Field
    case _: Defn.Macro => l.SymbolKind.Constructor
    case _: Defn.Class => l.SymbolKind.Class
    case _: Defn.Trait => l.SymbolKind.Interface
    case _: Defn.Object => l.SymbolKind.Module
    case _: Pkg.Object => l.SymbolKind.Namespace
    case _: Pkg => l.SymbolKind.Package
    case _: Type.Param => l.SymbolKind.TypeParameter
    case _: Lit.Null => l.SymbolKind.Null
    case _ => l.SymbolKind.Field
  }
}

private trait DocumentSymbolBuilder[A] {
  def apply(name: String, kind: l.SymbolKind, range: l.Range, uri: String): A
}
private object DocumentSymbolBuilder {
  implicit val documentSymbolBuilder: DocumentSymbolBuilder[l.DocumentSymbol] =
    (name, symbolKind, range, uri) =>
      new l.DocumentSymbol(name, symbolKind, range, range)

  implicit val symbolInformationBuilder: DocumentSymbolBuilder[l.SymbolInformation] =
    (name, symbolKind, range, uri) =>
      new l.SymbolInformation(name, symbolKind, new l.Location(uri, range))
}
