package scala.meta.internal.metals

final case class DocumentSymbolConfig(value: String) {
  def isDocumentSymbol: Boolean = value == "document-symbol"
  def isLegacy: Boolean = value == "symbol-information"
}

object DocumentSymbolConfig {
  def legacy = new DocumentSymbolConfig("symbol-information")
  def documentSymbol = new DocumentSymbolConfig("document-symbol")
  def default = new DocumentSymbolConfig(
    System.getProperty("metals.document-symbol", "document-symbol")
  )
}
