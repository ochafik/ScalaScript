package scalaxy.js

import org.junit._
import Assert._

import com.google.javascript.rhino.jstype.FunctionType

import com.google.javascript.rhino.Node
import com.google.javascript.jscomp._

class ExternsGeneratorTest {
  
  def test(expected: String, src: String) {
  	def rep(s: String) = s.trim.replaceAll("""(?m)^\s+""", "")
  	val x = rep(expected)
  	val p = ExternsGenerator.postProcessPrintedTree(src)
  	val s = rep(p)
  	if (x != s) {
  		println("Expected:\n" + expected + "\nActual:\n" + p)
  	}
  	assertEquals(x, s)
  }

  @Test
  def simple {
  	test(
  		"""
  			trait Foo extends scala.Any
	  		class ReferenceError(opt_message: Any, opt_file: Any, opt_line: Any) extends js.Error(opt_message, opt_file, opt_line) {
			  }
			""",
			"""
				trait Foo extends scala.Any
				class ReferenceError extends js.Error {
			   <paramaccessor> val opt_message$: Any = _;
			   <paramaccessor> val opt_file$: Any = _;
			   <paramaccessor> val opt_line$: Any = _;
			   def <init>(opt_message$: Any, opt_file$: Any, opt_line$: Any) = {
			     super.<init>(opt_message, opt_file, opt_line);
			     ()
			   }
			 }
		  """
	  )
  }
}
