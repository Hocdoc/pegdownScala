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

abstract class Node

class SuperNode(val children: List[Node] = Nil) extends Node
abstract class SuperText(val text: String) extends Node

case class RootNode(_children: List[Node]) extends SuperNode(_children)

case class TextNode(_text: String) extends SuperText(_text)
case class SpecialTextNode(_text: String) extends SuperText(_text)
case class VerbatimNode(_text: String, language: Option[String]) extends SuperText(_text)

case class ParaNode(child: Node) extends SuperNode(child :: Nil)
case class EmphNode(_children: List[Node]) extends SuperNode(_children)
case class StrongNode(_children: List[Node]) extends SuperNode(_children)

case class HeaderNode(level: Int, _children: List[Node]) extends SuperNode(_children)

// Some case classes instead of the SimpleNode Enumeration:
case class SimpleNodeApostrophe() extends Node
case class SimpleNodeEllipsis() extends Node
case class SimpleNodeEmdash() extends Node
case class SimpleNodeEndash() extends Node
case class SimpleNodeHRule() extends Node
case class SimpleNodeLinebreak() extends Node
case class SimpleNodeNbsp() extends Node

// Quotes
case class QuotedNodeDoubleAngle(_children: List[Node]) extends SuperNode(_children)
case class QuotedNodeDouble(_children: List[Node]) extends SuperNode(_children)
case class QuotedNodeSingle(_children: List[Node]) extends SuperNode(_children)

// Links
case class ExpImageNode(title: String, url: String, child: Node) extends SuperNode(child :: Nil)
case class ExpLinkNode(title: String, url: String, child: Node) extends SuperNode(child :: Nil)
case class RefImageNode(referenceKey: Option[SuperNode], separatorSpace: Option[String], child: Node) extends SuperNode(child :: Nil)
case class RefLinkNode(referenceKey: Option[SuperNode], separatorSpace: Option[String], child: Node) extends SuperNode(child :: Nil)

case class WikiLinkNode(_text: String) extends SuperText(_text)
case class AutoLinkNode(_text: String) extends SuperText(_text)
case class MailLinkNode(_text: String) extends SuperText(_text)

case class ReferenceNode(title: Option[String], url: String, child: Node) extends SuperNode(child :: Nil)
case class AbbreviationNode(expansion: SuperNode, child: Node) extends SuperNode(child :: Nil)

case class CodeNode(_text: String) extends SuperText(_text)

// Lists
case class DefinitionNode(_children: List[Node]) extends SuperNode(_children)
case class DefinitionListNode(_children: List[Node]) extends SuperNode(_children)
case class DefinitionTermNode(_children: List[Node]) extends SuperNode(_children)
case class ListItemNode(_children: List[Node]) extends SuperNode(_children)
case class BulletListNode(_children: List[ListItemNode]) extends SuperNode(_children)
case class OrderedListNode(_children: List[ListItemNode]) extends SuperNode(_children)

case class BlockQuoteNode(_children: List[Node]) extends SuperNode(_children)

// Html
case class HtmlBlockNode(_text: String) extends SuperText(_text)
case class InlineHtmlNode(_text: String) extends SuperText(_text)

// Tables
case class TableNode(header: Option[TableHeaderNode], columns: List[TableColumnNode], body: Option[TableBodyNode],  caption: Option[TableCaptionNode]) extends Node
case class TableHeaderNode(_children: List[TableRowNode]) extends SuperNode(_children)
case class TableBodyNode(_children: List[TableRowNode]) extends SuperNode(_children)
case class TableRowNode(_children: List[TableCellNode]) extends SuperNode(_children)
case class TableCaptionNode(_children: List[Node]) extends SuperNode(_children)
case class TableColumnNode(alignment: ColumnAlignment.Value, pipeSeen: Option[Boolean]) extends SuperNode(Nil)      // no child?
case class TableCellNode(colSpan: Int, pipeSeen: Boolean, _children: List[Node]) extends SuperNode(_children)

object ColumnAlignment extends Enumeration {
  val None, Left, Right, Center = Value
}

