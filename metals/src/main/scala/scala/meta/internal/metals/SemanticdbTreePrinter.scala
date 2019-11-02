package scala.meta.internal.metals

import scala.meta.internal.{semanticdb => s}

object SemanticdbTreePrinter {
  def printTree(t: s.Tree): String =
    t match {
      case s.Tree.Empty => ""
      case s.OriginalTree(range) => ""
      case s.TypeApplyTree(function, typeArguments) =>
        printTree(function) + printTypeArgs(typeArguments)
      case s.ApplyTree(function, arguments) =>
        printTree(function) + printArgs(arguments)
      case s.LiteralTree(constant) =>
        printCostant(constant)
      case s.SelectTree(qualifier, id) =>
        id.map(printTree).getOrElse("")
      case s.FunctionTree(parameters, body) =>
        printArgs(parameters) + "=>" + printTree(body)
      case s.IdTree(symbol) =>
        printSymbol(symbol)
      case s.MacroExpansionTree(beforeExpansion, tpe) =>
        printTree(beforeExpansion)
    }

  private def printSymbol(s: String): String =
    s.replaceAll("/", ".")
      .stripSuffix(".")
      .stripSuffix("#")
      .replaceAll("()", "")

  private def printPrefix(t: s.Type): String =
    printType(t) match {
      case "" => ""
      case s => s"$s."
    }

  private def printTypeArgs(typeArgs: Seq[s.Type]): String = typeArgs match {
    case Nil => ""
    case _ => typeArgs.map(printType).mkString("[", ", ", "]")
  }

  private def printArgs(args: Seq[s.Tree]): String = args match {
    case Nil => ""
    case _ => args.map(printTree).mkString("(", ", ", ")")
  }

  private def printType(t: s.Type): String = t match {
    case s.Type.Empty => ""
    case s.RepeatedType(tpe) =>
      s"${printType(tpe)}*"
    case s.SingleType(prefix, symbol) =>
      s"${printPrefix(prefix)}${printSymbol(symbol)}"
    case s.TypeRef(prefix, symbol, typeArguments) =>
      s"${printPrefix(prefix)}${printSymbol(symbol)}${printTypeArgs(typeArguments)}"
    case s.WithType(types) =>
      types.map(printType).mkString(" with ")
    case s.ConstantType(constant) =>
      printCostant(constant)
    case s.ByNameType(tpe) =>
      s"=> ${printType(tpe)}"
    case s.ThisType(symbol) =>
      s"this.${printSymbol(symbol)}"
    case s.IntersectionType(types) =>
      types.map(printType).mkString(" & ")
    case s.UnionType(types) =>
      types.map(printType).mkString(" | ")
    case s.SuperType(prefix, symbol) =>
      s"super.${printPrefix(prefix)}${printSymbol(symbol)}"
    case s.UniversalType(typeParameters, tpe) => "" // TODO(gabro)
    case s.AnnotatedType(annotations, tpe) => "" // TODO(gabro)
    case s.ExistentialType(tpe, declarations) => "" // TODO(gabro)
    case s.StructuralType(tpe, declarations) => "" // TODO(gabro)
  }

  private def printCostant(c: s.Constant): String = c match {
    case s.FloatConstant(value) => value.toString
    case s.LongConstant(value) => value.toString
    case s.DoubleConstant(value) => value.toString
    case s.NullConstant() => "null"
    case s.IntConstant(value) => value.toString
    case s.CharConstant(value) => value.toString
    case s.ByteConstant(value) => value.toString
    case s.UnitConstant() => "()"
    case s.ShortConstant(value) => value.toString
    case s.Constant.Empty => ""
    case s.BooleanConstant(value) => value.toString
    case s.StringConstant(value) => value

  }

}
