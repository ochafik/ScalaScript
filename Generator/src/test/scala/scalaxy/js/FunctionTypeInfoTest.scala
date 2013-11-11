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
  	val compiler = ClosureCompilerUtils.scanExterns(List(SourceFile.fromCode(srcName, src)))
    compiler -> compiler.scope.getVars.filter(_.getInputName.equals(srcName)).toList
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

  	for (v <- vs) {
  		println("VAR: " + v)
  		// println(s"\ttpe = ${v.getType}\n\tdoc = ${v.getJSDocInfo}")
  		// Option(v.getType) collect {
  		// 	case t: FunctionType =>
  		// 		println("\tparams = " + t.getParameters)
  		// }
  		println("\t" + SomeFunctionTypeInfo.unapply(v))
  		println("\t" + SomeFunctionTypeInfo.unapply(v.getType))
  	}

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
