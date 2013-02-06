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

/** Default values for the configuration of the Markdown parser. */
object MarkdownParserConfiguration {
    
  val noExtensions = MarkdownParserConfiguration()
  
  /** All available extensions excluding the SUPPRESS_... options. */
  val allExtensions = MarkdownParserConfiguration(  
    smarts = true,
    quotes = true,
    abbreviations = true,
    hardwraps = true,
    autolinks = true,
    tables = true,
    definitions = true,
    fencedCodeBlocks = true,
    wikilinks = true
  )
  
  /** ALL & ~SMARTYPANTS & ~HARDWRAPS */
  val commonExtensions = MarkdownParserConfiguration(
    smarts = false,
    quotes = false,
    abbreviations = true,
    hardwraps = false,
    autolinks = true,
    tables = true,
    definitions = true,
    fencedCodeBlocks = true,
    wikilinks = true
  )
  
  /** ALL & ~HARDWRAPS */
  val allWithoutHardwraps = MarkdownParserConfiguration(
    smarts = true,
    quotes = true,
    abbreviations = true,
    hardwraps = false,
    autolinks = true,
    tables = true,
    definitions = true,
    fencedCodeBlocks = true,
    wikilinks = true,
    buildParseTree = false
  )
  
}

/**
 * Configuration for the Markdown parser.
 * 
 * @param smarts Pretty ellipses, dashes and apostrophes.
 * 
 * @param quotes Pretty single and double quotes
 * 
 * @param abbreviations PHP Markdown Extra style abbreviations.
 *   @see <a href="http://michelf.com/projects/php-markdown/extra/#abbr">PHP Markdown Extra</a>
 *
 * @param hardwraps Enables the parsing of hard wraps as HTML linebreaks. Similar to what github does.
 *   @see <a href="http://github.github.com/github-flavored-markdown">Github-flavored-Markdown</a>
 * 
 * @þaram autolinks  Enables plain autolinks the way github flavoured markdown implements them.
 *   With this extension enabled pegdown will intelligently recognize URLs and email addresses
 *   without any further delimiters and mark them as the respective link type.
 *   @see <a href="http://github.github.com/github-flavored-markdown">Github-flavored-Markdown</a>
 *
 * @param tables Table support similar to what Multimarkdown offers.
 *   @see <a href="http://fletcherpenney.net/multimarkdown/users_guide/">MultiMarkdown</a>
 *
 * @param definitions PHP Markdown Extra style definition lists.
 *   Additionally supports the small extension proposed in the article referenced below.
 *   @see <a href="http://michelf.com/projects/php-markdown/extra/#def-list">PHP Markdown Extra</a>
 *   @see <a href="http://www.justatheory.com/computers/markup/modest-markdown-proposal.html">Extension proposal</a>
 *
 * @param fencedCodeBlocks PHP Markdown Extra style fenced code blocks.
 *   @see <a href="http://michelf.com/projects/php-markdown/extra/#fenced-code-blocks">PHP Markdown Extra</a>
 *
 * @param wikilinks Support [[Wiki-style links]]. URL rendering is performed by the active {@link LinkRenderer}.
 *   @see <a href="http://github.github.com/github-flavored-markdown">Github-flavored-Markdown</a>
 *
 * @param supressHtmlBlocks Suppresses HTML blocks. They will be accepted in the input but not be contained in the output.
 * 
 * @param supressInlineHtml Suppresses inline HTML tags. They will be accepted in the input but not be contained in the output.
 * 
 * @param buildParseTree Generate a parse tree for debugging the grammar.
 */
case class MarkdownParserConfiguration(
  smarts: Boolean = false,
  quotes: Boolean = false,
  abbreviations: Boolean = false,
  hardwraps: Boolean = false,
  autolinks: Boolean = false,
  tables: Boolean = false,
  definitions: Boolean = false,
  fencedCodeBlocks: Boolean = false,
  wikilinks: Boolean = false,
  supressHtmlBlocks: Boolean = false,
  supressInlineHtml: Boolean = false,
  buildParseTree: Boolean = false
)