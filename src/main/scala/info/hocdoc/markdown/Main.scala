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

import scala.io.Source

object Main extends App { 
  
  if(args.isEmpty || args.head == "--help") {
    println("pegdownScala - A markdown processor written in Scala.\n" +
      "Usage: pegdownScala <MarkdownFilenames.md>")
  } else {
    args.map(markdownFileToHtml)
  }
  
  def markdownFileToHtml(filename: String) = {
    val source = Source.fromFile(filename).mkString
    val html = ToHtmlSerializer(source)
    println(html)    
  }
  
}