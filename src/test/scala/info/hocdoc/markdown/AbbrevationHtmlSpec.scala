package info.hocdoc.markdown

import java.io.{StringWriter, StringReader}
import org.specs2.mutable._
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class AbbreviationHtmlSpec extends Specification {

  val testAbb = new TestAbb
  
  "AbbreviationHtml" should {

    val html = "Jesus loves you.<p Jesus> Jesus is love.Jesus joJesus jep"

    "check if an index is inside a tag" in {
      testAbb.isTag(html, 2) === false
      testAbb.isTag(html, 17) === true
      testAbb.isTag(html, 18) === true
      testAbb.isTag(html, 24) === true
      testAbb.isTag(html, 25) === false 
    }

    "check if a range is whole word" in {
      testAbb.isWholeWord(html, 0, 3) === false
      testAbb.isWholeWord(html, 0, 4) === true
      testAbb.isWholeWord(html, 6, 10) === true
      testAbb.isWholeWord(html, 6, 9) === false
    }

    "find all" in {
      val xs = testAbb.findAll(html, "Jesus", 0)
      xs === 0 :: 19 :: 26 :: 40 :: 48 :: Nil
    }
    
    "find all indices to expand" in {
      val xs = testAbb.findAllIndicesToExpand(html, "Jesus")
      xs === 0 :: 26 :: 40 :: Nil
    }

  }
}

class TestAbb extends AbbrevationHtml {
  val root = null
  def nodeToHtml(node: Node) = throw new NotImplementedError 
}
