Introduction
------------

_pegdownScala_ is a pure Scala library for clean and lightweight [Markdown] processing based on a [parboiled] PEG parser.

_pegdownScala_ is nearly 100% compatible with the original Markdown specification and fully passes the original Markdown test suite.
On top of the standard Markdown feature set _pegdownScala_ implements a number of extensions similar to what other popular Markdown processors offer.  
Currently _pegdownScala_ supports the following extensions over standard Markdown:

* SMARTS: Beautifies apostrophes, ellipses ("..." and ". . .") and dashes ("--" and "---")
* QUOTES: Beautifies single quotes, double quotes and double angle quotes (&laquo; and &raquo;)
* SMARTYPANTS: Convenience extension enabling both, SMARTS and QUOTES, at once.
* ABBREVIATIONS: Abbreviations in the way of [PHP Markdown Extra](http://michelf.com/projects/php-markdown/extra/#abbr).
* HARDWRAPS: Alternative handling of newlines, see [Github-flavoured-Markdown]
* AUTOLINKS: Plain (undelimited) autolinks the way [Github-flavoured-Markdown] implements them.
* TABLES: Tables similar to [MultiMarkdown](http://fletcherpenney.net/multimarkdown/) (which is in turn like the [PHP Markdown Extra](http://michelf.com/projects/php-markdown/extra/#table) tables, but with colspan support).
* DEFINITION LISTS: Definition lists in the way of [PHP Markdown Extra](http://michelf.com/projects/php-markdown/extra/#def-list).
* FENCED CODE BLOCKS: Fenced Code Blocks in the way of [PHP Markdown Extra](http://michelf.com/projects/php-markdown/extra/#fenced-code-blocks) or [Github-flavoured-Markdown].
* HTML BLOCK SUPPRESSION: Suppresses the output of HTML blocks.
* INLINE HTML SUPPRESSION: Suppresses the output of inline HTML elements.
* WIKILINKS: Support `[[Wiki-style links]]` with a customizable URL rendering logic.

Note: _pegdownScala_ differs from the original Markdown in that it ignores in-word emphasis as in

    > my_cool_file.txt
    > 2*3*4=5

Currently this "extension" cannot be switched off.

Differences to pegdown
----------------------

_pegdownScala_ is a port of the great [PegDown] Java library from Mathias Doenitz. 
There are some differences to the Java Version.

Pros:

* The parser is implemented as immutable class and thread safe.
* Should run on the Google AppEngine.
* More compact Scala code (30% of the Java code lines).
* No time expensive parser extension step.

Cons:

* pegdownScala is a beta version: interlaced HTML tags doesn't work correct, 
  PegDown is much better tested.
* No implementation for timeouts in pegdownScala. You may want to use Akka to handle long tasks.
* pegdownScala isn't optimized for performance.

Usage
-----

Using _pegdownScala_ is very simple: Just call `ToHtmlSeralizer(markdownSource)`. 

TODO: Add more documentation.

You can also use _pegdownScala_ from the command line to convert a Markdown file to HTML:

`pegdownScala <MarkdownFilename.md>`

Credits
-------

All the code is based on [PegDown] from Mathias Doenitz. I would never be able to build this
Scala port without your great work.

A large part of the underlying PEG grammar was developed by John MacFarlane and made available with his
tool [peg-markdown](http://github.com/jgm/peg-markdown).   

License
-------

_pegdownScala_ is licensed under [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0).

  
   [Markdown]: http://daringfireball.net/projects/markdown/ "Main Markdown site"
   [parboiled]: http://www.parboiled.org
   [Github-flavoured-Markdown]: http://github.github.com/github-flavored-markdown/
   [MultiMarkdown]: http://fletcherpenney.net/multimarkdown/users_guide/multimarkdown_syntax_guide/
   [Download Page]: http://github.com/sirthias/pegdown/downloads
   [PegDown]: https://github.com/sirthias/pegdown
   [PegDownProcessor]: http://www.decodified.com/pegdown/api/org/pegdown/PegDownProcessor.html
   [LinkRenderer]: http://www.decodified.com/pegdown/api/org/pegdown/LinkRenderer.html
   [Visitor]: http://www.decodified.com/pegdown/api/org/pegdown/ast/Visitor.html
   [ToHtmlSerializer]: https://github.com/sirthias/pegdown/blob/master/src/main/java/org/pegdown/ToHtmlSerializer.java
   [idea-markdown plugin]: https://github.com/nicoulaj/idea-markdown
   [SBT]: http://www.scala-sbt.org/
   [Node]: http://www.decodified.com/pegdown/api/org/pegdown/ast/Node.html
