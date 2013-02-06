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

import java.net.URLEncoder

object LinkRenderer {

  def render(node: Node, htmlRender: ToHtmlSerializer, refNode: ReferenceNode = null): LinkRenderer = node match {
    case n: AutoLinkNode => LinkRenderer(n.text, n.text)
    case n: ExpLinkNode => LinkRenderer(n.url, htmlRender.nodeToHtml(n.children), title(n.title))
    case n: MailLinkNode => {
      val obfuscated = FastEncoder.obfuscate(n.text)
      LinkRenderer("mailto:" + obfuscated, obfuscated)
    }
    case n: RefLinkNode => LinkRenderer(refNode.url, htmlRender.nodeToHtml(n.children), title(refNode.title.getOrElse("")))
    case n: WikiLinkNode => {
      val url = "./" + URLEncoder.encode(n.text.replace(' ', '-'), "UTF-8") + ".html"
      LinkRenderer(url, n.text)
    }
    case _ => throw new IllegalArgumentException("Unknown node " + node)
  }    
  
  private def title(text: String): List[Attribute] =
    if(text.isEmpty)
      Nil
    else
      Attribute("title", FastEncoder.encode(text)) :: Nil    
    
}

case class LinkRenderer(href: String, text: String, attributes: List[Attribute] = Nil)

object Attribute {
  val noFollow = Attribute("rel", "nofollow")
}
  
case class Attribute(name: String, value: String)

