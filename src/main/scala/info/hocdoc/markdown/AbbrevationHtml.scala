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

trait AbbrevationHtml {

  val root: RootNode 
  def nodeToHtml(node: Node): String
  
  def expandAbbreviations(html: String): String = allAbbreviationNodes(root) match {
    case Nil                        => html
    case xs: List[AbbreviationNode] => 
      xs.foldLeft(html){(text, node) => expandAbbreviation(text, node)}
  }
  
  private def expandAbbreviation(html: String, node: AbbreviationNode): String = {
    val nodeText = nodeToHtml(node.child)
    val indicesToExpand = findAllIndicesToExpand(html, nodeText)
    val expansion = nodeToHtml(node.expansion)
    expandAbbreviation(html, indicesToExpand, nodeText, expansion, 0)
  }
  
  def findAllIndicesToExpand(html: String, searchText: String): List[Int] =
    if(searchText.isEmpty)
      Nil
    else
      findAll(html, searchText).filter(x => 
        !isTag(html, x) && isWholeWord(html, x, x + searchText.length - 1)
      )
  
  private def expandAbbreviation(html: String, replaceIndices: List[Int], searchText: String, 
    expansion: String, fromIndex: Int): String = replaceIndices match {
      case x :: xs => 
        html.substring(fromIndex, x) +
        expansionToHtml(searchText, expansion) +
        expandAbbreviation(html, xs, searchText, expansion, x + searchText.length)
      case Nil => html.substring(fromIndex)
    }
  
  /** Generate an abbr-HTML tag from an expansion. */
  private def expansionToHtml(abbr: String, title: String): String =
    "<abbr" + 
    (if(title.isEmpty) "" else " title=\"" + FastEncoder.encode(title) + "\"") +
    ">" + abbr + "</abbr>"
  
  /** Find all whole words of the searchText in the wholeText. */
  def findAll(wholeText: String, searchText: String, fromIndex: Int = 0): List[Int] = 
    wholeText.indexOf(searchText, fromIndex) match {
      case -1     => Nil
      case n: Int => n :: findAll(wholeText, searchText, n + 1)
    }
  
  private def allAbbreviationNodes(node: Node): List[AbbreviationNode] = node match {
    case n: AbbreviationNode => n :: Nil
    case n: SuperNode => n.children.flatMap(allAbbreviationNodes(_)) 
    case _ => Nil
  }
 
  /** Returns true, if the index is inside an HTML tag. */
  def isTag(html: String, index: Int): Boolean =
    html.charAt(index) == '<' ||
    html.substring(index).find(x => (x == '<' || x == '>')) == Some('>')

  /** Returns true, if the given range isn't only a part of a word. */
  def isWholeWord(html: String, beginIndex: Int, endIndex: Int): Boolean =
    ((beginIndex == 0 || !Character.isLetterOrDigit(html.charAt(beginIndex - 1)))) &&
    ((endIndex == html.length - 1) || !Character.isLetterOrDigit(html.charAt(endIndex + 1)))
}