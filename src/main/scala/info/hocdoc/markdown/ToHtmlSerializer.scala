/*
 * Copyright (C) 2013 Bernhard Berger
 * 
 * Based on pegdown (C) 2010-2011 Mathias Doenitz and
 * peg-markdown (C) 2008-2010 John MacFarlane.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package info.hocdoc.markdown

import org.parboiled.scala._
import org.parboiled.errors._

object ToHtmlSerializer {
  
  def apply(markdown: String, parser: MarkdownParser = MarkdownParser.allWithoutHardwraps): String = {
    val parsingResult = ReportingParseRunner(parser.Root).run(markdown + "\n\n")
    
    if(parser.buildParseTree) {
      val parseTreePrintOut = org.parboiled.support.ParseTreeUtils.printNodeTree(parsingResult)
      println(parseTreePrintOut + "\n\n")       // Just for debugging
    }
        
    parsingResult.result match {
      case Some(astRoot) => (new ToHtmlSerializer(astRoot, parser.config)).toHtml
      case None          => throw new ParsingException("Invalid Markdown:\n" +
        ErrorUtils.printParseErrors(parsingResult))
    }
  }
}

class ToHtmlSerializer(val root: RootNode, val config: MarkdownParserConfiguration) extends AbbrevationHtml {
  
  val references: Map[String, ReferenceNode] = allReferenceNodes(root).toMap  
  
  private def allReferenceNodes(node: Node): List[(String, ReferenceNode)] = node match {
    case n: ReferenceNode => {
      // References could not be interlaced, so we can be sure that nodeToHtml doesn't need
      // the references map.
      val key = normalize(nodeToHtml(n.child))
      (key, n) :: Nil
    }
    case n: SuperNode => n.children.flatMap(allReferenceNodes(_)) 
    case _ => Nil
  }
  
  def toHtml: String = {
    val html = nodeToHtml(root)
    if(config.abbreviations)
      expandAbbreviations(html)
    else
      html
  }
  
  def nodeToHtml(node: Node): String = node match {
    case n: AbbreviationNode => ""
    case n: AutoLinkNode => link(LinkRenderer.render(n, ToHtmlSerializer.this));
    case n: BlockQuoteNode => indentedTag(n, "blockquote")
    case n: BulletListNode => indentedTag(n, "ul")
    case n: CodeNode => tag(n, "code")
    case n: DefinitionListNode => indentedTag(n, "dl")
    case n: DefinitionNode => indentedTag(n, "dd")
    case n: DefinitionTermNode => indentedTag(n, "dt")
    case n: EmphNode => tag(n, "em")
    case n: ExpImageNode => imageTag(n, n.url)         // TODO: Title-Tag?
    case n: ExpLinkNode => link(LinkRenderer.render(n, ToHtmlSerializer.this));
    case n: HeaderNode => tag(n, "h" + n.level)
    case n: HtmlBlockNode => if(n.text.isEmpty) "" else "\n" + n.text
    case n: InlineHtmlNode => n.text    
    case n: ListItemNode => tag(n, "li")
    case n: MailLinkNode => link(LinkRenderer.render(n, ToHtmlSerializer.this));
    case n: OrderedListNode => indentedTag(n, "ol")
    case n: ParaNode => tag(n, "p")
    case n: QuotedNodeDoubleAngle => "&laquo;" + nodeToHtml(n.children) + "&raquo;"    
    case n: QuotedNodeDouble => "&ldquo;" + nodeToHtml(n.children) + "&rdquo;"    
    case n: QuotedNodeSingle => "&lsquo;" + nodeToHtml(n.children) + "&rsquo;"    
    case n: ReferenceNode => ""     // reference nodes are not printed
    case n: RefImageNode => refImageNode(n)
    case n: RefLinkNode => refLinkNode(n)
    case n: SimpleNodeApostrophe => "&rsquo;"
    case n: SimpleNodeEllipsis => "&hellip;"
    case n: SimpleNodeEmdash => "&mdash;"
    case n: SimpleNodeEndash => "&ndash;"
    case n: SimpleNodeHRule => "<hr/>"
    case n: SimpleNodeLinebreak => "<br/>"          
    case n: SimpleNodeNbsp => "&nbsp;"          
    case n: StrongNode => tag(n, "strong")
    case n: TableNode => table(n)
    case n: VerbatimNode => verbatimNodeToHtml(n)
    case n: WikiLinkNode => link(LinkRenderer.render(n, ToHtmlSerializer.this));
    case n: TextNode => n.text
    case n: SpecialTextNode => FastEncoder.encode(n.text)
    case n: RootNode => nodeToHtml(n.children)
    case n: SuperNode => nodeToHtml(n.children)
    case _ => throw new RuntimeException("Not implemented " + node)
  }
  
  def verbatimNodeToHtml(node: VerbatimNode): String = {
    // print HTML breaks for all initial newlines
    var text = node.text
    val firstEmptyLines = new StringBuilder()
    while(text.charAt(0) == '\n') { 
      firstEmptyLines.append("<br/>")
      text = text.substring(1)
    }
    
    "<pre><code" + 
    node.language.map(x => attribute("class", x)).getOrElse("") +
    ">\n" + firstEmptyLines.toString + 
    FastEncoder.encode(text) +
    "\n</code></pre>"
  }
    
  def nodeToHtml(nodes: List[Node]): String = nodes.map(n => nodeToHtml(n)).mkString
  
  protected def indentedTag(node: SuperNode, tag: String): String =
    "<" + tag + ">" + nodeToHtml(node.children) + "</" + tag + ">"

  protected def tag(node: SuperNode, tag: String): String =
    "<" + tag + ">" + nodeToHtml(node.children) + "</" + tag + ">"

  protected def tag(node: SuperText, tag: String): String =
    "<" + tag + ">" + FastEncoder.encode(node.text) + "</" + tag + ">"

  protected def attribute(name: String, value: String): String =
    " " + name + "=\"" + value + "\""
    
  protected def imageTag(node: SuperNode, url: String): String =
    "<img src=\"" + url + "\"  alt=\"" + FastEncoder.encode(nodeToHtml(node.children)) + "\"/>"
    
  protected def link(rendering: LinkRenderer): String =
    "<a" + attribute("href", rendering.href) + 
    rendering.attributes.map(a => attribute(a.name, a.value)).mkString + ">" +
    rendering.text + "</a>"
  
  private def refImageNode(node: RefImageNode): String = {
    val text = nodeToHtml(node.children)
    val key = node.referenceKey.map(nodeToHtml(_)).getOrElse(text)
    references.get(normalize(key)) match {
      case None => { // "fake" reference image link
        "![" + text + "]" + 
        node.separatorSpace.map(x => x + "[" + node.referenceKey.map(_ => key).getOrElse("") + "]").getOrElse("")
      }
      case Some(refNode) => imageTag(node, refNode.url)
    }
  }

  private def refLinkNode(node: RefLinkNode): String = {
    val text = nodeToHtml(node.children)
    val key = node.referenceKey.map(nodeToHtml(_)).getOrElse(text)
    references.get(normalize(key)) match {
      case None => { // "fake" reference image link
        "[" + text + "]" + 
        node.separatorSpace.map(x => x + "[" + node.referenceKey.map(_ => key).getOrElse("") + "]").getOrElse("")
      }
      case Some(refNode) => link(LinkRenderer.render(node, ToHtmlSerializer.this, refNode))
    }    
  }
  
  private def table(table: TableNode): String = {
    def childrenToContent(inTableHeader: Boolean, children: List[TableRowNode]) =
      "  <" +
      (if(inTableHeader) "thead>" else "tbody>") +
      children.flatMap(x => tableRow(x, table.columns, inTableHeader)).mkString +
      "  </" +
      (if(inTableHeader) "thead>" else "tbody>")
    
    val header = table.header.map(x => childrenToContent(true, x._children)).getOrElse("")
    val body = table.body.map(x => childrenToContent(false, x._children)).getOrElse("")      
    val caption = table.caption.map(indentedTag(_, "caption")).getOrElse("")
      
    "\n<table>\n" + header + body + caption + "\n</table>\n"
  }
  
  private def tableRow(node: TableRowNode, columns: List[TableColumnNode], inTableHeader: Boolean): String = 
    "\n    <tr>\n" + 
    zipCellsWithColumns(node._children, columns).map(x => tableCell(x._1, x._2, inTableHeader)).mkString +
    "    </tr>\n"
  
  /** Zip a list of rows with columns. Different colSpan is supported. */
  private def zipCellsWithColumns(cells: List[TableCellNode], columns: List[TableColumnNode]):
    List[(TableCellNode, Option[TableColumnNode])] = cells match {
    case x :: xs => (x, columns.headOption) :: zipCellsWithColumns(xs, columns.drop(x.colSpan))
    case Nil => Nil
  }
 
  private def tableCell(node: TableCellNode, column: Option[TableColumnNode], inTableHeader: Boolean): String = {
    val tag = if(inTableHeader) "th" else "td"
    val colSpanAttribute = if(node.colSpan > 1) (" colspan=\"" + node.colSpan + "\"") else ""
    val alignAttribute = tableColumn(column)
    val content = nodeToHtml(node.children)
    s"      <$tag$alignAttribute$colSpanAttribute>$content</$tag>\n"
  }
  
  private def tableColumn(node: Option[TableColumnNode]): String = node.map{
    _.alignment match {
      case ColumnAlignment.Left => " align=\"left\""
      case ColumnAlignment.Center => " align=\"center\""
      case ColumnAlignment.Right => " align=\"right\""
      case _ => ""
    }
  }.getOrElse("")
  
  /** Returns the String without whitespaces and in lower case. */
  private def normalize(string: String): String = string.replaceAll("\\s", "").toLowerCase
  
}