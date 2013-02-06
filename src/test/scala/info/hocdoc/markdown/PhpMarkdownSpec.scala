package info.hocdoc.markdown

class PhpMarkdownSpec extends AbstractPegDownSpec {

  "The PegDownProcessor" should {

    "pass selected parts of the PhpMarkdown test suite" in {
      implicit val parser = MarkdownParser.noExtensions

      test("PhpMarkdown/Backslash_escapes")
//      test("PhpMarkdown/Code_block_in_a_list_item")  // TODO2
      test("PhpMarkdown/Code_Spans")
      test("PhpMarkdown/Email_auto_links")
//    test("PhpMarkdown/Emphasis")
//      test("PhpMarkdown/Headers")
      test("PhpMarkdown/Horizontal_Rules")  // TODO
//      test("PhpMarkdown/Inline_HTML_(Simple)")  // TODO
//      test("PhpMarkdown/Inline_HTML_(Span)")  // TODO
//      test("PhpMarkdown/Inline_HTML_comments")  // TODO
////      test("PhpMarkdown/Ins_and_del")
////      test("PhpMarkdown/Links_inline_style")
//      test("PhpMarkdown/MD5_Hashes")  // TODO
//      test("PhpMarkdown/Nesting")  // TODO
////      test("PhpMarkdown/Parens_in_URL")
////      test("PhpMarkdown/PHP-Specific_Bugs")
//      test("PhpMarkdown/Tight_blocks")  // TODO
    }

    "pass selected parts of the PhpMarkdownExtra test suite" in {
      implicit val parser = MarkdownParser.commonExtensions

      test("PhpMarkdownExtra/Abbr")
//      test("PhpMarkdownExtra/Definition_Lists")  // TODO
//      test("PhpMarkdownExtra/Fenced_Code_Blocks")  // TODO
//      test("PhpMarkdownExtra/Tables")  // TODO
    }
  }

}
