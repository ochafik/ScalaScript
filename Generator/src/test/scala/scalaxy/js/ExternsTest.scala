package scalaxy.js

import org.junit._
import Assert._

import com.google.javascript.rhino.jstype.FunctionType

import com.google.javascript.rhino.Node
import com.google.javascript.jscomp._

class ExternsTest {
  
  def conv(src: String): String =
  	ExternsGenerator.generate(
  		List(SourceFile.fromCode("test.js", src)),
  		ownerName = "js",
  		filter = v => v.getInputName.equals("test.js"))

  def checkConversion(externs: String, scalaSource: String) {
  	def rep(s: String) = s.trim.replaceAll("""(?m)^\s+""", "")

  	val c = conv(externs)
  	// println("CONV: " + c)
  	val r = rep(c)
  	// println("REP: " + r)
  	assertEquals(rep(scalaSource), r)
  }
  // @Test
  def simple {
  	checkConversion(
  		""" /** @constructor */
          var MyClass = function() {};

	        /** @this {!MyClass}
	          * @param {number} a
	          * @param {string} b
	          * @param {?number} c
	          * @param {number=} opt_d
	          * @return {number}
	          * @override
	          */
	        MyClass.prototype.f = function(a, b, c, opt_d) {};

	        /** @type {function(number, string, ?number, number=): number}
	          * @deprecated
	          */
	        MyClass.prototype.g = function(a, b, c, opt_d) {};

	        /** @param {*} message */ 
	        var alertMe = function(message) {};
      """,
  		""" object js {
	  				@scalaxy.js.global
		  			class MyClass extends js.Object {
		  				override def f(a: Double, b: String, c: java.lang.Double, opt_d: scala.Option[Double] = None): Double = ???
		  				@scala.deprecated() def g(a: Double, b: String, c: java.lang.Double, opt_d: scala.Option[Double] = None): Double = ???
		  			};
		  			def alertMe(message: Any): Any = ???
	  			}
  		""")
  }
  // @Test
  def constructors {
  	checkConversion(
  		""" /** @constructor
            * @param {!Array.<number>} a
            */
          var MyClass = function(a) {};
      """,
  		""" object js {
		  			@scalaxy.js.global
		  			class MyClass(a$: js.Array[Double]) extends js.Object {
		  			}
	  			}
  		""")
  }
  // @Test
  def classTemplates {
  	checkConversion(
  		""" /** @constructor
            * @template T
            */
          var MyClass = function() {};
      """,
  		""" object js {
		  			@scalaxy.js.global
		  			class MyClass[T] extends js.Object {
		  			}
	  			}
  		""")
  }
  @Test
  def methodTemplates {
  	checkConversion(
  		""" /** @constructor
            */
          var MyClass = function() {};

          /** @this {MyClass}
            * @param {U} u
            * @template U
            */
          MyClass.prototype.f = function(u) {};
      """,
  		""" object js {
		  			@scalaxy.js.global
		  			class MyClass extends js.Object {
	  					def f[U](u: U): Any = ???
		  			}
	  			}
  		""")
  }
}
