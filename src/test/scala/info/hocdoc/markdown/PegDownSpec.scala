package info.hocdoc.markdown

class PegDownSpec extends AbstractPegDownSpec {

  "The PegDownProcessor" should {

    "pass the custom pegdown tests for all extensions" in {
      def runSuite()(implicit parser: MarkdownParser) {
        test("pegdown/Abbreviations")
        test("pegdown/AttributeWithUnderscore")
        test("pegdown/Autolinks")
        test("pegdown/Bug_in_0.8.5.1")
//        test("pegdown/Bug_in_0.8.5.4")    // TODO: Code-li-Bug
        test("pegdown/Bug_in_1.0.0")
        test("pegdown/Bug_in_1.1.0")
        test("pegdown/GFM_Fenced_Code_Blocks")
        test("pegdown/Linebreaks")
        test("pegdown/Parens_in_URL")
        test("pegdown/Quoted Blockquote")
        test("pegdown/Smartypants")
        test("pegdown/Tables")
        test("pegdown/Wikilinks")

// The scala version of Pegdown doesn't build ASTs the same way as the Java version:        
//        testAST("pegdown/AstText")
//        testAST("pegdown/GFM_Fenced_Code_Blocks")
      }

      "with the default parser" in {
        implicit val parser = MarkdownParser.allExtensions 
        runSuite
      }

// TODO: ?
//      "with a custom parser" in {
//        runSuite(new PegDownProcessor(Parboiled.createParser[CustomParser, AnyRef](classOf[CustomParser])))
//      }
    }

    "pass the custom pegdown tests for no extensions" in {
      implicit val parser = MarkdownParser.noExtensions

      test("pegdown/Emph_With_Linebreaks")
      test("pegdown/Special Chars")
    }

    "pass the HTML suppression test" in {
      "without suppression" in {
        test("pegdown/HTML suppression",
          """<h1>HTML <b>SUPPRESSION</b></h1>
            |<p>This is a paragraph containing a <strong>strong</strong> inline
            |HTML element and:</p>
            |<div>
            |<p>an actual block of HTML!</p>
            |</div>
            |
            |""".stripMargin
        )(new MarkdownParser(MarkdownParserConfiguration(supressHtmlBlocks = false, supressInlineHtml = false)))
      }
      "with inline suppression" in {
        test("pegdown/HTML suppression",
          """<h1>HTML SUPPRESSION</h1>
            |<p>This is a paragraph containing a strong inline HTML element
            |and:</p>
            |<div>
            |<p>an actual block of HTML!</p>
            |</div>
            |
            |""".stripMargin
        )(new MarkdownParser(MarkdownParserConfiguration(supressHtmlBlocks = false, supressInlineHtml = true)))
      }
      "with block suppression" in {
        test("pegdown/HTML suppression",
          """<h1>HTML <b>SUPPRESSION</b></h1>
            |<p>This is a paragraph containing a <strong>strong</strong> inline
            |HTML element and:</p>
            |
            |""".stripMargin
        )(new MarkdownParser(MarkdownParserConfiguration(supressHtmlBlocks = true, supressInlineHtml = false)))
      }
      "with block and inline suppression" in {
        test("pegdown/HTML suppression",
          """<h1>HTML SUPPRESSION</h1>
            |<p>This is a paragraph containing a strong inline HTML element
            |and:</p>
            |
            |""".stripMargin
        )(new MarkdownParser(MarkdownParserConfiguration(supressHtmlBlocks = true, supressInlineHtml = true)))
      }
    }
  }

}

//class CustomParser extends Parser(ALL, 1000, Parser.DefaultParseRunnerProvider)