package info.hocdoc.markdown

class Markdown103Spec extends AbstractPegDownSpec {

  "The PegDownProcessor" should {

    "pass the Markdown test suite" in {
      def runMarkdownTestSuite(implicit parser: MarkdownParser) {
        test("MarkdownTest103/Amps and angle encoding")
        test("MarkdownTest103/Auto links")
        test("MarkdownTest103/Backslash escapes")
        test("MarkdownTest103/Blockquotes with code blocks")
        test("MarkdownTest103/Code Blocks")
        test("MarkdownTest103/Code Spans")
        test("MarkdownTest103/Hard-wrapped paragraphs with list-like lines")
        test("MarkdownTest103/Horizontal rules")
//        test("MarkdownTest103/Inline HTML (Advanced)")    // TODO
//        test("MarkdownTest103/Inline HTML (Simple)")      // TODO
        test("MarkdownTest103/Inline HTML comments")
        test("MarkdownTest103/Links, inline style")
        test("MarkdownTest103/Links, reference style")
        test("MarkdownTest103/Links, shortcut references")
        test("MarkdownTest103/Literal quotes in titles")
        test("MarkdownTest103/Nested blockquotes")
        test("MarkdownTest103/Ordered and unordered lists")
        test("MarkdownTest103/Strong and em together")
        test("MarkdownTest103/Tabs")
        test("MarkdownTest103/Tidyness")

        test("MarkdownTest103/Markdown Documentation - Basics")   // TODO
        test("MarkdownTest103/Markdown Documentation - Syntax")   // TODO
      }

      "without any enabled extensions" in {
        runMarkdownTestSuite(MarkdownParser.noExtensions)
      }

      "with most extensions enabled" in {
        runMarkdownTestSuite(MarkdownParser.commonExtensions)
      }
    }
  }

}
