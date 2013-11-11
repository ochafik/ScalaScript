package scalaxy.js

import org.junit._
import Assert._

import scala.collection.JavaConversions._

import com.google.javascript.rhino.jstype._

import com.google.javascript.rhino.Node
import com.google.javascript.jscomp._

object FunctionTypeInfoTest {
	def vars(src: String): (ClosureCompiler, List[Scope.Var]) = {
  	val srcName = "test.js"
  	implicit val compiler = ClosureCompilerUtils.scanExterns(List(SourceFile.fromCode(srcName, src)))
  	val vs = compiler.scope.getVars.filter(_.getInputName.equals(srcName)).toList
  	// for (v <- vs) println("VAR: " + v +"\n\t" + SomeFunctionTypeInfo.unapply(v))
    compiler -> vs
  }
}
class FunctionTypeInfoTest {

  import FunctionTypeInfoTest._

  @Test
  def simple {
  	implicit val (compiler, vs) = vars("""
  		/**
  		 * @constructor
  		 * @param {number} x
  		 * @param {string} y
  		 */
  		var C = function(x, y) {};

  		/**
  		 * @this {!C}
  		 * @param {number} x
  		 * @param {string} y
  		 * @return {Function}
  		 */
  		C.prototype.f = function(x, y) {};

  		/**
  		 * @type {function(number, string): string}
  		 */
  		C.prototype.g = function(x, y) {};

  		C.prototype.h = function(x, y) {};

  		/**
  		 * @type {function(number, string): string}
  		 */
  		C.prototype.i;
		""")
		import compiler._

    val List(c, cproto, f, g, h, i) = vs

    {
	    val SomeFunctionTypeInfo(info) = f
	    val List(("x", x), ("y", y)) = info.params
	    assertEquals(Nil, info.templateParams)
	    assertEquals(None, info.thisTemplateParams)
	    assertTrue(x.isNumber)
	    assertTrue(y.isString)
	  }

    {
	    val SomeFunctionTypeInfo(info) = g
	    val List(("x", x), ("y", y)) = info.params
	    assertEquals(Nil, info.templateParams)
	    assertEquals(None, info.thisTemplateParams)
	    assertTrue(x.isNumber)
	    assertTrue(y.isString)
	  }

    {
	    val SomeFunctionTypeInfo(info) = h
	    val List(("x", null), ("y", null)) = info.params
	    assertEquals(Nil, info.templateParams)
	    assertEquals(None, info.thisTemplateParams)
	  }

    {
	    val SomeFunctionTypeInfo(info) = i
	    val List(("param0", x), ("param1", y)) = info.params
	    assertEquals(Nil, info.templateParams)
	    assertEquals(None, info.thisTemplateParams)
	    assertTrue(x.isNumber)
	    assertTrue(y.isString)
	  }
  }

  @Test
  def simple2 {
  	try {
	  	implicit val (compiler, vs) = vars("""
	      /** @constructor */
	      var MyClass = function() {};

	      /**
	        * @this {!MyClass}
	        * @param {number} a
	        * @param {string} b
	        * @param {?number} opt_c
	        * @return {number}
	        */
	      MyClass.prototype.f = function(a, b, opt_c) {};

	      /**
	        * @type {function(number, string, ?number): number}
	        */
	      MyClass.prototype.g = function(a, b, opt_c) {};
	    """)

	    val List(c, cproto, f, g) = vs

	    {
		    val SomeFunctionTypeInfo(info) = f
		    val List(("a", a), ("b", b), ("opt_c", c)) = info.params
		    assertEquals(Nil, info.templateParams)
		    assertEquals(None, info.thisTemplateParams)
		    assertTrue("not a number: " + a, a.isNumber)
		    assertTrue("not a string: " + b, b.isString)
		    // assertTrue("not a number: " + c, c.isNumber)// && c.isOptional)
		  }

	    {
		    val SomeFunctionTypeInfo(info) = g
		    val List(("a", a), ("b", b), ("opt_c", c)) = info.params
		    assertEquals(Nil, info.templateParams)
		    assertEquals(None, info.thisTemplateParams)
		    assertTrue("not a number: " + a, a.isNumber)
		    assertTrue("not a string: " + b, b.isString)
		    // assertTrue("not a number: " + c, c.isNumber)// && c.isOptional)
		  }
	  } catch { case ex: Throwable =>
	  	ex.printStackTrace()
	  	throw ex
	  }
  }

  @Test
  def testTemplateParams {
  	implicit val (compiler, List(f)) = vars("""
  		/**
  		 * @this {{length: number}|Array.<T>}
  		 * @param {T} x
  		 * @param {U} y
  		 * @return {Function}
  		 * @template T, U
  		 */
  		Array.prototype.f = function(x, y) {};
		""")
		import compiler._

  	val SomeFunctionTypeInfo(info) = f
  	val List(("x", x), ("y", y)) = info.params
  	val List("T", "U") = info.templateParams
  	val Some(List("T")) = info.thisTemplateParams
  }
}
