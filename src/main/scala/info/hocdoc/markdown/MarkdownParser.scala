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
import org.parboiled._

object MarkdownParser {
  lazy val noExtensions = new MarkdownParser(MarkdownParserConfiguration.noExtensions)
  lazy val allExtensions = new MarkdownParser(MarkdownParserConfiguration.allExtensions)
  lazy val commonExtensions = new MarkdownParser(MarkdownParserConfiguration.commonExtensions)
  lazy val allWithoutHardwraps = new MarkdownParser(MarkdownParserConfiguration.allWithoutHardwraps)
}

class MarkdownParser(val config: MarkdownParserConfiguration) extends Parser {
  
  override val buildParseTree = config.buildParseTree
  
  private def parseInternal(source: String): RootNode = {
    val parsingResult = ReportingParseRunner(Root).run(source + "\n\n")

    parsingResult.result match {
      case Some(astRoot) => astRoot
      case None          => throw new ParsingException("Invalid Markdown:\n" + ErrorUtils.printParseErrors(parsingResult))
    }
  }
 
  /** Used together with the ~~% operator to drop an element from the value stack. */
  def drop(node: Any) = { 
    // Do nothing
  }
  
  /** Always fails, but has the type to return an Node. Used for parser extensions. */
  def Fail: Rule1[Node] = rule { NOTHING ~> TextNode }

  //************* BLOCKS ****************

  def Root: Rule1[RootNode] = rule { zeroOrMore(Block) ~~> RootNode }
  
  def Block: Rule1[Node] = rule { 
    zeroOrMore(BlankLine) ~ 
    (
      BlockQuote | Verbatim | 
      (if(config.abbreviations) Abbreviation else Fail) |
      Reference | HorizontalRule | Heading | OrderedList | BulletList | HtmlBlock |
      (if(config.tables) Table else Fail) |
      (if(config.definitions) DefinitionList else Fail) | 
      (if(config.fencedCodeBlocks) FencedCodeBlock else Fail) |
      Para | Inlines  
    ) }

  def BlockQuote: Rule1[BlockQuoteNode] = rule { 
    oneOrMore(
      ">" ~ optional(" ") ~ Line ~
      zeroOrMore(!(">") ~ !(BlankLine) ~ Line) ~
      zeroOrMore(BlankLine ~> (_.toString)) ~~>
      ((x: String, ys: List[String], zs: List[String]) => x :: ys ::: zs)
    ) ~~>
    // trigger a recursive parsing run on the inner source we just built
    // and attach the root of the inner parses Node
    ((xs: List[List[String]]) => BlockQuoteNode(parseInternal(xs.flatten.mkString("\n")).children))
  }

  def Verbatim: Rule1[VerbatimNode] = rule {
    oneOrMore(
      zeroOrMore(BlankLine ~ push("")) ~
      Indent ~ oneOrMore(NotNewline ~ ANY) ~> (_.toString) ~ Newline ~~>
      ((xs: List[String], y: String) => xs ::: tabsToSpaces(y) :: Nil)
    ) ~~> ((xs: List[List[String]]) => VerbatimNode(xs.flatten.mkString("\n"), None))
  }

  /** Replace all tabs with 1 to 4 spaces. */
  private def tabsToSpaces(string: String): String = tabsToSpaces(0, string.split("\t").toList).mkString

  private def tabsToSpaces(beginIndex: Int, notExpanded: List[String]): List[String] = notExpanded match {
    case x :: Nil => notExpanded
    case x :: xs => {
      val currentIndex = beginIndex + x.length
      val spacesCount = 4 - currentIndex % 4
      x + " " * spacesCount :: tabsToSpaces(currentIndex + spacesCount, xs)
    }
    case Nil => Nil
  }

  def FencedCodeBlock: Rule1[VerbatimNode] = rule {
    CodeFence ~
    optional(oneOrMore(!(Newline) ~ ANY) ~> (_.toString)) ~ // GFM code type identifier
    Newline ~
    !(CodeFence) ~          // prevent empty matches
    zeroOrMore(BlankLine ~ push("")) ~            // lines: List[String]
    oneOrMore(!(Newline ~ CodeFence) ~ ANY) ~> (_.toString) ~ 
    Newline ~~> 
    ((language: Option[String], xs: List[String], y: String) => VerbatimNode(tabsToSpaces(xs.mkString("\n") + y), language)) ~
    CodeFence
  }

  def CodeFence: Rule0 = rule { ("~~~" ~ zeroOrMore("~")) | ("```" ~ zeroOrMore("`")) }

  def Para: Rule1[ParaNode] = rule { NonindentSpace ~ Inlines ~~> ParaNode ~ oneOrMore(BlankLine)}
 
  def HorizontalRule: Rule1[SimpleNodeHRule] = rule {
    NonindentSpace ~ 
    (HorizontalRule("*") | HorizontalRule("-") | HorizontalRule("_")) ~
    Sp ~ Newline ~ oneOrMore(BlankLine) ~
    push(SimpleNodeHRule())
  }

  // See issue https://github.com/sirthias/parboiled/issues/40
  def HorizontalRule(character: String): Rule0 = {
    character ~ Sp ~ character ~ Sp ~ character ~ zeroOrMore(Sp, character)
  }

  //************* HEADINGS ****************

  def Heading: Rule1[HeaderNode] = rule { AtxHeading | SetextHeading }
 
  def AtxHeading: Rule1[HeaderNode] = rule { 
    AtxStart ~ optional(Sp) ~ oneOrMore(AtxInline) ~~> ((level, children) => HeaderNode(level, children)) ~ optional(Sp ~ zeroOrMore("#") ~ Sp) ~ Newline
  }
 
  def AtxStart: Rule1[Int] = rule { ("######" | "#####" | "####" | "###" | "##" | "#") ~> (_.length) }
 
  def AtxInline: Rule1[Node] = rule { 
    !Newline ~ !(optional(Sp) ~ zeroOrMore("#") ~ Sp ~ Newline) ~ Inline 
  }

  def SetextHeading: Rule1[HeaderNode] = rule {
    // test for successful setext heading before actually building it to reduce backtracking
    &(oneOrMore(NotNewline ~ ANY) ~ Newline ~ (("===" ~ zeroOrMore("=")) | ("---" ~ zeroOrMore("-"))) ~ Newline) ~
    (SetextHeading1 | SetextHeading2)
  }

  def SetextHeading1: Rule1[HeaderNode] = rule { 
    oneOrMore(SetextInline) ~ Newline ~ "===" ~ zeroOrMore("=") ~ Newline ~~> (c => HeaderNode(1, c)) 
  }
  
  def SetextHeading2: Rule1[HeaderNode] = rule { 
    oneOrMore(SetextInline) ~ Newline ~ "---" ~ zeroOrMore("-") ~ Newline ~~> (c => HeaderNode(2, c)) 
  }

  def SetextInline = rule { 
    !(Endline) ~ Inline 
  }

  //************** Definition Lists ************
   
  def DefinitionList: Rule1[DefinitionListNode] = rule {
    // test for successful definition list match before actually building it to reduce backtracking
    !(Spacechar) ~
    &(oneOrMore(!(BlankLine) ~ !(DefListBullet) ~ oneOrMore(NotNewline ~ ANY) ~ Newline) ~
      optional(BlankLine) ~ DefListBullet
    ) ~
    oneOrMore(
      oneOrMore(DefListTerm) ~
      oneOrMore(Definition) ~
      optional(BlankLine) ~~> ((xs: List[DefinitionTermNode], ys: List[DefinitionNode]) => xs ::: ys)
    ) ~~> ((xs: List[List[Node]]) => DefinitionListNode(xs.flatten))
  }

  def DefListTerm: Rule1[DefinitionTermNode] = rule {
    !(Spacechar) ~ !(DefListBullet) ~
    oneOrMore(DefTermInline) ~~> (DefinitionTermNode(_)) ~
    optional(":") ~ Newline
  }

  def DefTermInline: Rule1[Node] = rule { NotNewline ~ !(":" ~ Newline) ~ Inline }

  def Definition: Rule1[DefinitionNode] = rule { ListItem(DefListBullet, false) ~~> (x => DefinitionNode(x.children))}

  def DefListBullet: Rule0 = rule { NonindentSpace ~ anyOf(":~") ~ oneOrMore(Spacechar) }

  //************* LISTS ****************

  def BulletList: Rule1[BulletListNode] = rule {
    ListItem(Bullet, true) ~ zeroOrMore(ListItem(Bullet, false)) ~ zeroOrMore(BlankLine) ~~> 
    ((x: ListItemNode, ys: List[ListItemNode]) => BulletListNode(x :: ys))
  }

  def OrderedList: Rule1[OrderedListNode] = rule {
    ListItem(Enumerator, true) ~ zeroOrMore(ListItem(Enumerator, false)) ~ zeroOrMore(BlankLine) ~~>
    ((x: ListItemNode, ys: List[ListItemNode]) => OrderedListNode(x :: ys))
  }

  // See issue https://github.com/sirthias/parboiled/issues/40
  def ListItem(itemStart: Rule0, isFirstItem: Boolean): Rule1[ListItemNode] = {
    // for a simpler parser design we use a recursive parsing strategy for list items:
    // we collect a number of markdown source blocks for an item, run complete parsing cycle on these and attach
    // the roots of the inner parsing results Node to the outer Node tree
    optional(BlankLine ~ push(false)) ~     // tight Boolean
    itemStart ~ Line ~                    // block String = first line of the list item
    zeroOrMore(optional(Indent) ~ NotItem ~ Line) ~ // temp   List[String] = first paragraph of the list item, excluding the first line
    zeroOrMore(optional(BlankLine ~ push(false)) ~ Indent ~ (DoubleIndentedBlocks | IndentedBlock)) ~    // List[(Option[Boolean], String)
    optional(&(oneOrMore(BlankLine) ~ itemStart) ~ push(false)) ~~>   // tight after
    ((tightBefore: Option[Boolean], x: String, ys: List[String], zs: List[(Option[Boolean], String)], tightAfter: Option[Boolean]) => {
      val firstParagraph = (x :: ys).mkString("\n")
      val allParagraphs = firstParagraph :: zs.unzip._2
      val internalNodes = parseInternal(allParagraphs.mkString("\n\n")).children

      val itemContainsBlanklines = zs.exists(_._1 == Some(false))
      val tight = !itemContainsBlanklines && ((!isFirstItem && tightBefore.getOrElse(true)) || (isFirstItem && tightAfter.getOrElse(true)))
      val itemChildren = if(tight) removeHeadParagraph(internalNodes) else internalNodes  // Remove the paragraph from tight lists

      ListItemNode(itemChildren)
    })
  }

  /** 
   * Removes a paragraph at the head of a nodes list. 
   * If no paragraph exists, the same node list is returned.
   */
  def removeHeadParagraph(nodes: List[Node]): List[Node] = nodes.head match {
    // A Newline is added to the SuperNode to support the tests from other Markdown parsers.
    case n: ParaNode => new SuperNode(n.children ::: TextNode("\n") :: Nil) :: nodes.tail
    case _ => nodes
  }

  def DoubleIndentedBlocks: Rule1[String] = rule {
    Indent ~ !(BlankLine) ~ Line ~        // String
    zeroOrMore(
      zeroOrMore(BlankLine ~ (push(""))) ~
      Indent ~ Indent ~  Line  ~~> 
      ((xs: List[String], y: String) => (xs ::: ("    " + y) :: Nil))
    ) ~~> // List[List[String]]
    ((x: String, ys: List[List[String]]) => "    " + (x :: ys.flatten).mkString("\n"))
  }

  def IndentedBlock: Rule1[String] = rule {
    Line ~ 
    zeroOrMore(
      ((!(BlankLine) ~ Indent) | NotItem) ~ Line
    ) ~~>
    ((x: String, xs: List[String]) => (x :: xs).mkString("\n"))
  }

  def NotItem: Rule0 = rule {
    !(Bullet | Enumerator | BlankLine | (HorizontalRule ~~% (drop(_))) | ((if(config.definitions) DefinitionList else Fail) ~~% (drop(_))))
  }

  def Enumerator: Rule0 = rule { NonindentSpace ~ oneOrMore(Digit) ~ "." ~ oneOrMore(Spacechar) }

  def Bullet: Rule0 = rule { !(HorizontalRule) ~ NonindentSpace ~ anyOf("+*-") ~ oneOrMore(Spacechar) }

  //************* HTML BLOCK ****************

  def HtmlBlock: Rule1[HtmlBlockNode] = rule {
    (HtmlTagBlockAnyTag | HtmlComment | HtmlBlockSelfClosing) ~>
    (s => HtmlBlockNode(if(config.supressHtmlBlocks) "" else s)) ~
    oneOrMore(BlankLine)
  }

  def HtmlTagBlockAnyTag: Rule0 = rule {
    val tags = List(
      "address", "blockquote", "center", "dd", "dir", "div", "dl", "dt", "fieldset", "form", "frameset", "h1",
      "h2", "h3", "h4", "h5", "h6", "hr", "isindex", "li", "menu", "noframes", "noscript", "ol", "p", "pre",
      "script", "style", "table", "tbody", "td", "tfoot", "th", "thead", "tr", "ul")
    
    &("<") ~
    tags.map(HtmlTagBlock(_)).reduce(_ | _)
  }

  // See issue https://github.com/sirthias/parboiled/issues/40
  def HtmlTagBlock(tagName: String): Rule0 = {
    HtmlBlockOpen(tagName) ~
    zeroOrMore( /* HtmlTagBlock(tagName) | */   // This doesn't work because we cannot use `rule`
      (!(HtmlBlockClose(tagName)) ~ ANY)) ~
    HtmlBlockClose(tagName)
  }

  def HtmlBlockSelfClosing: Rule0 = rule {
    "<" ~ Spn1 ~ HtmlBlockTagName ~ Spn1 ~ zeroOrMore(HtmlAttribute) ~ optional("/") ~ Spn1 ~ ">"
  }

  def HtmlBlockOpenAnyTag: Rule1[String] = rule {
    "<" ~ Spn1 ~ (HtmlBlockTagName ~> (x => {println(s"Open: $x"); x.toString})) ~ Spn1 ~ zeroOrMore(HtmlAttribute) ~ ">" ~ run(println("HtmlBlockOpenAnyTag: done"))
  }

  // See issue https://github.com/sirthias/parboiled/issues/40
  def HtmlBlockOpen(tagName: String): Rule0 = {
    "<" ~ Spn1 ~ tagName ~ Spn1 ~ zeroOrMore(HtmlAttribute) ~ ">"
  }
    
  // See issue https://github.com/sirthias/parboiled/issues/40
  def HtmlBlockClose(tagName: String): Rule0 = {
    "<" ~ Spn1 ~ "/" ~ tagName ~ Spn1 ~ ">"
  }

  def HtmlBlockTagName: Rule0 = rule {
    "address" | "blockquote" | "center" | "dd" | "dir" | "div" | "dl" | "dt" | "fieldset" | "form" | "frameset" | "h1" |
    "h2" | "h3" | "h4" | "h5" | "h6" | "hr" | "isindex" | "li" | "menu" | "noframes" | "noscript" | "ol" | "p" | "pre" |
    "script" | "style" | "table" | "tbody" | "td" | "tfoot" | "th" | "thead" | "tr" | "ul"
  }

  //************* INLINES ****************

  def Inlines: Rule1[SuperNode] = rule { 
    oneOrMore(InlineOrIntermediateEndline) ~~> (new SuperNode(_)) ~ optional(Endline ~~% drop _) 
  }
  
  def InlineOrIntermediateEndline: Rule1[Node] = rule { 
    (!(Endline) ~ Inline) | (Endline ~ &(Inline)) 
  }

  def Inline: Rule1[Node] = rule(MemoMismatches) { Link | NonLinkInline }

  def NonAutoLinkInline = rule { NonAutoLink | NonLinkInline }
  
  def NonLinkInline: Rule1[Node] = rule { 
    Str | Endline | UlOrStarLine | Space | StrongOrEmph | Image | Code | InlineHtml | Entity | EscapedChar | 
    (if(config.quotes) (SingleQuoted | DoubleQuoted | DoubleAngleQuoted) else Fail) |
    (if(config.smarts) Smarts else Fail) |
    Symbol
  }

  def Endline: Rule1[Node] = rule(MemoMismatches) { 
    (LineBreak | TerminalEndline | NormalEndline) 
  }

  def LineBreak: Rule1[SimpleNodeLinebreak] = rule { 
    "  " ~ NormalEndline ~~> (x => SimpleNodeLinebreak()) 
  }
  
  def TerminalEndline: Rule1[TextNode] = rule { Sp ~ Newline ~ &(EOI) ~ push(TextNode("\n"))}
  
  def NormalEndline: Rule1[Node] = rule { 
    Sp ~ Newline ~ !(
      BlankLine | ">" | (AtxStart ~~% drop _) | (zeroOrMore(NotNewline, ANY) ~ Newline ~
        ("===" ~ zeroOrMore("=")) | 
        ("---" ~ zeroOrMore("-")) | 
        Newline)
    ) ~ push(if(config.hardwraps) SimpleNodeLinebreak() else TextNode(" "))
  }

  //************* EMPHASIS / STRONG ****************

  def UlOrStarLine: Rule1[TextNode] = rule(MemoMismatches) { (CharLine("_") | CharLine("*")) ~> TextNode  }

  // See issue https://github.com/sirthias/parboiled/issues/40
  def CharLine(char: String): Rule0 = {((char * 4) ~ zeroOrMore(char)) | (Spacechar ~ oneOrMore(char)  ~ &(Spacechar))}
    
  def StrongOrEmph: Rule1[SuperNode] = rule { 
    &(anyOf("*_")) ~ (Strong | Emph) 
  }
  
  def Emph: Rule1[EmphNode] = rule { 
    (EmphOrStrong("*") | EmphOrStrong("_")) ~~> EmphNode 
  }

  def Strong: Rule1[StrongNode] = rule { 
    (EmphOrStrong("__") | EmphOrStrong("**")) ~~> StrongNode 
  }

  // See issue https://github.com/sirthias/parboiled/issues/40
  def EmphOrStrong(chars: String): Rule1[List[Node]] = { 
    EmphOrStrongOpen(chars) ~ 
    oneOrMore(!(EmphOrStrongClose(chars)) ~ Inline) ~
    EmphOrStrongClose(chars) 
  }

  // See issue https://github.com/sirthias/parboiled/issues/40
  def EmphOrStrongOpen(chars: String): Rule0 = { !(CharLine(chars.head.toString)) ~ chars ~ !(Spacechar) ~ NotNewline }

  // See issue https://github.com/sirthias/parboiled/issues/40
  def EmphOrStrongClose(chars: String): Rule0 = { !(Spacechar) ~ NotNewline ~ chars ~ !(Alphanumeric) }

  //************* LINKS ****************

  def Image: Rule1[Node] = rule { 
    "!" ~ Label ~ (ExplicitLink(true) | ReferenceLink(true)) 
  }

  def Link: Rule1[Node] = rule(MemoMismatches) { 
    (if(config.wikilinks) WikiLink else Fail) |
    (Label ~ (ExplicitLink(false) | ReferenceLink(false))) | AutoLink
  }

  def NonAutoLink: Rule1[Node] = rule { 
    Label ~ (ExplicitLink(false) | ReferenceLink(false)) 
  }
    
  // See issue https://github.com/sirthias/parboiled/issues/40
  def ExplicitLink(image: Boolean): ReductionRule1[Node, Node] = { 
    Spn1 ~ "(" ~ Sp ~ LinkSource ~ Spn1 ~ (LinkTitle | push("")) ~ Sp ~ ")" ~~>
    ((child: Node, url: String, title: String) => 
      if(image) ExpImageNode(title, url, child) else ExpLinkNode(title, url, child)) }

  // See issue https://github.com/sirthias/parboiled/issues/40
  def ReferenceLink(image: Boolean): ReductionRule1[Node, Node] = {
    ((Spn1 ~> (x => Some(x.toString))             // separatorSpacecOption[String]
      ~ (Label | ("[]" ~ push(null)))     // referenceKey SuperNode
    ) |
    (push(None) ~ push(null))) ~~>
    ((child: Node, separatorSpace: Option[String], referenceKey: SuperNode) => 
      if(image) 
        RefImageNode(Option(referenceKey), separatorSpace, child) 
      else 
        RefLinkNode(Option(referenceKey), separatorSpace, child)) 
  }

  def LinkSource: Rule1[String] = rule {
    ("(" ~ LinkSource ~ ")") |
    ("<" ~ LinkSource ~ ">") |
    (
      oneOrMore(
        ("\\" ~ anyOf("()")) ~> (_.toString) |
      (!(anyOf("()>")) ~ Nonspacechar ~> (_.toString))
      ) ~~> (_.mkString)
    ) |
    push("")
  }

  def LinkTitle: Rule1[String] = rule { LinkTitle("'") | LinkTitle("\"") }

  // See issue https://github.com/sirthias/parboiled/issues/40
  def LinkTitle(delimiter: String): Rule1[String] = { 
    delimiter ~
    zeroOrMore(!(delimiter ~ Sp ~ (")" | Newline)) ~ NotNewline ~ ANY) ~> (_.toString) ~ delimiter
  }

  def AutoLink: Rule1[Node] = rule { 
    (if(config.autolinks) optional("<") else str("<")) ~ 
    (AutoLinkUrl | AutoLinkEmail) ~ 
    (if(config.autolinks) optional(">") else str(">"))
  }

  def WikiLink: Rule1[WikiLinkNode] = rule { "[[" ~ oneOrMore(!"]" ~ ANY) ~> WikiLinkNode ~ "]]" }

  def AutoLinkUrl: Rule1[AutoLinkNode] = rule { 
    oneOrMore(Letter) ~> (_.toString) ~ 
    "://" ~ AutoLinkEnd ~> (_.toString) ~~> 
    ((x: String, y: String) => AutoLinkNode(x + "://" + y)) 
  }

  def AutoLinkEmail: Rule1[MailLinkNode] = rule { 
    oneOrMore(Alphanumeric | anyOf("-+_.")) ~> (_.toString) ~ "@" ~ 
    AutoLinkEnd ~> (_.toString) ~~> 
    ((x: String, y: String) => MailLinkNode(x + "@" + y))
  }

  def AutoLinkEnd: Rule0 = rule {
    oneOrMore(!Newline ~ 
      !(if(config.autolinks) (">" | optional(anyOf(".,;:)}]\"'")) ~ (Spacechar | Newline)) else str(">")) ~ 
      ANY
    )
  }

  //************* REFERENCE ****************

  def Label: Rule1[SuperNode] = rule { 
    "[" ~ oneOrMore(!("]") ~ NonAutoLinkInline) ~~> (new SuperNode(_)) ~ "]" 
  }

  def Reference: Rule1[ReferenceNode] = rule {
    NonindentSpace ~ Label ~
    ":" ~ Spn1 ~ RefSrc ~
    Sp ~ optional(RefTitle) ~
    Sp ~ Newline ~ zeroOrMore(BlankLine) ~~> 
    ((child: SuperNode, src: String, title: Option[String]) => ReferenceNode(title, src, child))
  }

  def RefSrc: Rule1[String] = rule {
    ("<" ~ RefSrcContent ~ ">") | RefSrcContent
  }

  def RefSrcContent: Rule1[String] = rule {
    oneOrMore(!(">") ~ Nonspacechar) ~> (_.toString)
  }

  def RefTitle: Rule1[String] = rule {
    RefTitle("'", "'") | RefTitle("\"", "\"") | RefTitle("(", ")")
  }

  // See issue https://github.com/sirthias/parboiled/issues/40
  def RefTitle(open: String, close: String): Rule1[String] = {
    open ~  zeroOrMore(!(close ~ Sp ~ Newline) ~ NotNewline ~ ANY) ~> (_.toString) ~ close
  }
 
  //************* CODE ****************

  // See issue https://github.com/sirthias/parboiled/issues/40
  def Code: Rule1[CodeNode] = rule { 
    &("`") ~ 
    (Code(Ticks(1)) | Code(Ticks(2)) |  Code(Ticks(3)) | Code(Ticks(4)) | Code(Ticks(5)))
  }


  // See issue https://github.com/sirthias/parboiled/issues/40
  def Code(ticks: Rule0): Rule1[CodeNode] = {
    ticks ~ Sp ~
    oneOrMore(
      (!("`") ~ Nonspacechar) |
      (!(ticks) ~ oneOrMore("`")) |
      (!(Sp ~ ticks) ~ (Spacechar | (Newline ~ !(BlankLine))))
    ) ~> (CodeNode(_)) ~ Sp ~ ticks
  }

  // See issue https://github.com/sirthias/parboiled/issues/40
  def Ticks(count: Int): Rule0 = {
    ("`" * count) ~ !("`")
  }
  
  //************* RAW HTML ****************

  def InlineHtml: Rule1[InlineHtmlNode] = rule { (HtmlComment | HtmlTag) ~> (x => InlineHtmlNode(if(config.supressInlineHtml) "" else x)) }

  def HtmlComment: Rule0 = rule { "<!--" ~ zeroOrMore(!("-->") ~ ANY) ~ "-->" }

  def HtmlTag: Rule0 = rule {
    "<" ~ Spn1 ~ optional("/") ~ oneOrMore(Alphanumeric) ~ Spn1 ~ zeroOrMore(HtmlAttribute) ~ optional("/")  ~ Spn1 ~ ">"
  }

  def HtmlAttribute: Rule0 = rule {
    oneOrMore(Alphanumeric | "-" | "_") ~ Spn1 ~ optional("=" ~ Spn1 ~ (Quoted | oneOrMore(!(">") ~ Nonspacechar))) ~ Spn1
  }

  def Quoted: Rule0 = rule {
    ("\"" ~ zeroOrMore(!("\"") ~ ANY) ~ "\"") |
    ("'" ~ zeroOrMore(!("'") ~ ANY) ~ "'")
  }
    
  //************* LINES ****************

  def BlankLine: Rule0 = rule { Sp ~ Newline }
 
  def Line: Rule1[String] = rule { zeroOrMore(NotNewline ~ ANY) ~> (_.toString) ~ Newline }
    
  //************* ENTITIES ****************

  def Entity = rule { "&" ~ (HexEntity | DecEntity | CharEntity) ~> (x => TextNode("&" + x + ";")) ~ ";" }
  
  def HexEntity = rule { "#" ~ ("x" | "X") ~ oneOrMore(Digit | anyOf("abcdefABCDEF")) }

  def DecEntity = rule { "#" ~ oneOrMore(Digit) }
  
  def CharEntity: Rule0 = rule { oneOrMore(Alphanumeric) }
 
  //************* BASICS ****************

  def Str = rule { oneOrMore(NormalChar) ~> TextNode }
  
  def Space = rule { oneOrMore(Spacechar) ~ push(TextNode(" ")) }

  def Spn1 = rule { Sp ~ optional(Newline ~ Sp) }

  def Sp: Rule0 = rule { zeroOrMore(Spacechar) }
  
  def Spacechar = rule { anyOf(" \t") }

  def Nonspacechar = rule { !(Spacechar) ~ NotNewline ~ ANY }

  def NormalChar = rule(MemoMismatches) { !(SpecialChar) ~ !(Spacechar) ~ NotNewline ~ ANY }

  def EscapedChar = rule { "\\" ~ anyOf("*_`&[]<>!#\\'\".+-(){}:|~") ~> SpecialTextNode }

  def Symbol = rule { SpecialChar ~> SpecialTextNode }

  def SpecialChar = rule {
    val quotes = if(config.quotes) "'\"" else ""
    val smarts = if(config.smarts) ".-" else ""
    val autolinks = if(config.autolinks) "(){}" else ""
    val definitions = if(config.definitions) ":" else ""
    val tables = if(config.tables) "|" else ""
    val definitionsOrFencedCodeBlocks = if(config.definitions || config.fencedCodeBlocks) "~" else ""

    anyOf("*_`&[]<>!#\\" + quotes + smarts + autolinks + definitions + tables + definitionsOrFencedCodeBlocks) 
  }

  def NotNewline = rule { !anyOf("\n\r") }
  
  def Newline: Rule0 = rule { "\n" | "\r" ~ optional("\n") }

  def NonindentSpace: Rule0 = rule { "   " | "  " | " " | EMPTY }
  
  def Indent = rule { "\t" | "    " }

  def Alphanumeric = rule { Letter | Digit }

  def Letter = rule { "a" - "z" | "A" - "Z" }

  def Digit = rule { "0" - "9" }

  //************* ABBREVIATIONS ****************

  def Abbreviation: Rule1[AbbreviationNode] = rule {
    NonindentSpace ~ "*" ~ Label ~ Sp ~ ":" ~ Sp ~ AbbreviationText ~ zeroOrMore(BlankLine) ~~> 
    ((child: Node,  expansion: SuperNode) => AbbreviationNode(expansion, child))
  }

  def AbbreviationText: Rule1[SuperNode] = rule { 
    zeroOrMore(NotNewline ~ Inline) ~~> (new SuperNode(_)) 
  }

  //************* TABLES ****************
  
  def Table: Rule1[TableNode] = rule {
    optional(oneOrMore(TableRow) ~~> (TableHeaderNode(_))) ~            // Option[TableHeaderNode]
    TableDivider ~                                                    // List[TableColumnNode]
    optional(oneOrMore(TableRow) ~~> (TableBodyNode(_))) ~            // Option[TableBodyNode]
    optional(TableCaption) ~~~?                                       // Option[TableCaptionNode]
    // only accept as table if we have at least one header or at least one body
    ((header: Option[TableHeaderNode], columns: List[TableColumnNode], body:  Option[TableBodyNode],  caption: Option[TableCaptionNode]) => 
      (header != None) || (columns != Nil) || (body != None) || (caption != None)
    ) ~~>
    ((header: Option[TableHeaderNode], columns: List[TableColumnNode], body:  Option[TableBodyNode], caption: Option[TableCaptionNode]) => 
      TableNode(header, columns, body, caption))
  }

  def TableCaption: Rule1[TableCaptionNode] = rule {
    "[" ~ optional(Sp) ~ oneOrMore(CaptionInline) ~ optional(Sp ~ optional("]") ~ Sp) ~ Newline ~~> (TableCaptionNode(_))
  }

  def CaptionInline: Rule1[Node] = rule { 
    !(Newline) ~ !(optional(Sp) ~ optional("]") ~ Sp ~ Newline) ~ Inline 
  }

  def TableDivider: Rule1[List[TableColumnNode]] = rule {
    optional("|" ~ push(true)) ~                     // pipeSeen: Option[Boolean]
    oneOrMore(TableColumn) ~~~?
    ((pipeSeen: Option[Boolean], columns: List[TableColumnNode]) =>
      (pipeSeen != None) || (columns.size > 1) || columns.exists(_.pipeSeen != None)) ~~>
    ((pipeSeen: Option[Boolean], columns: List[TableColumnNode]) => columns) ~
    Sp ~ Newline
  }

  def TableColumn: Rule1[TableColumnNode] = rule {
    Sp ~
    optional(":" ~ push(ColumnAlignment.Left)) ~     // markLeftAligned
    Sp ~ oneOrMore("-") ~ Sp ~
    optional(":" ~ push(ColumnAlignment.Right)) ~    // markRightAligned
    Sp ~
    optional("|" ~ push(true)) ~~>
    ((alignmentLeft: Option[ColumnAlignment.Value], alignmentRight: Option[ColumnAlignment.Value], pipeSeen: Option[Boolean]) => {
      var alignment = alignmentLeft match {
        case None => alignmentRight.getOrElse(ColumnAlignment.None)
        case _    => if(alignmentRight == None) ColumnAlignment.Left else ColumnAlignment.Center
      }
      TableColumnNode(alignment, pipeSeen)      // no Child?!
    })
  }

  def TableRow: Rule1[TableRowNode] = rule {
    optional("|" ~ push(true)) ~              // leadingPipe : Option[Boolean]
    oneOrMore(TableCell) ~~> (TableRowNode(_)) ~~~?
    ((leadingPipe: Option[Boolean], row: TableRowNode) =>
      (leadingPipe != None) || (row._children.size > 1) || row._children.last.pipeSeen) ~~>
    ((leadingPipe: Option[Boolean], row: TableRowNode) => row) ~
    Sp ~ Newline
  }

  def TableCell: Rule1[TableCellNode] = rule {
    !(Sp ~ optional(":") ~ Sp ~ oneOrMore("-") ~ Sp ~ optional(":") ~ Sp ~ ("|" | Newline)) ~
    optional(Sp ~ !("|") ~ NotNewline) ~
    oneOrMore(!("|") ~ !(Sp ~ Newline) ~ Inline ~ optional(Sp ~ &("|") ~ &(Newline))) ~     // List[Node]
    zeroOrMore("|" ~ push(true)) ~> 
    (x => math.max(1, x.length)) ~~>           // colSpan: Int
    ((children: List[Node], pipeSeenList: List[Boolean], colSpan: Int) => TableCellNode(colSpan, pipeSeenList.nonEmpty, children))
  }
  
  //************* SMARTS ****************

  def Smarts: Rule1[Node] = rule {
    (("..." | ". . .") ~ push(SimpleNodeEllipsis())) |
    ("---" ~ push(SimpleNodeEmdash())) |
    ("--" ~ push(SimpleNodeEndash())) |
    ("'" ~ push(SimpleNodeApostrophe()))
  }

  //************* QUOTES ****************

  def SingleQuoted: Rule1[QuotedNodeSingle] = rule {
    (lastCharacterWasNotALetter _) ~
    "'" ~ oneOrMore(!(SingleQuoteEnd) ~ Inline) ~ SingleQuoteEnd ~~> (QuotedNodeSingle(_))
  }

  def lastCharacterWasNotALetter(ctx: Context[Any]): Boolean = {
    val index = ctx.getCurrentIndex
    (index == 0) || !Character.isLetter(ctx.getInputBuffer.charAt(ctx.getCurrentIndex - 1))    
  }

  def SingleQuoteEnd: Rule0 = rule { "'" ~ !(Alphanumeric) }

  def DoubleQuoted: Rule1[QuotedNodeDouble] = rule { 
    "\"" ~ oneOrMore(!("\"") ~ Inline) ~ "\"" ~~> (QuotedNodeDouble(_)) 
  }

  def DoubleAngleQuoted: Rule1[QuotedNodeDoubleAngle] = rule { 
    "<<" ~ 
    optional(Spacechar ~ push(SimpleNodeNbsp())) ~
    oneOrMore(
      (oneOrMore(Spacechar) ~ &(">>") ~  push(SimpleNodeNbsp())) |
      (!(">>") ~ Inline)
    ) ~ ">>" ~~>
    ((x: Option[SimpleNodeNbsp], ys: List[Node]) => QuotedNodeDoubleAngle(x.map(_ :: ys).getOrElse(ys)))
  }
}
