/*
echo "window.console.log('yay');" | java -jar build/compiler.jar --warning_level VERBOSE --jscomp_error checkTypes
*/
package scalaxy.js

import java.io.File
import com.google.javascript.jscomp.ClosureCompilerUtils
import com.google.javascript.jscomp.SourceFile
import com.google.javascript.jscomp.Scope

import scala.reflect.runtime.{ universe => ru }
import scala.collection.mutable
import scala.collection.JavaConversions._

object ExternsGenerator extends App {

	// Make the Scala pretty-printed trees prettier (compilable).
  def postProcessPrintedTree(s: String): String = {
		s
			.replaceAll("extends scala.AnyRef ", "")
	  	.replaceAll("@new ", "@")
	  	.replaceAll(""" >: Nothing <: Any""", "")
	  	.replaceAll("""Any @deprecated\("record","([^"]+)"\)""", "$1")
	  	//.replaceAll("""(?m)(?:class|trait|object) (?:\w+) extends [\w.]+$""", "$0 {\n}")
	  	.replaceAll("""(?m)(?s)(class|trait|object) (\w+)(\[\w+(?:, \w+)*\])? extends ([^ \n]+) ([^\n{]*?\{).*?def (?:<init>|\$init\$)(\([^)]*\)) = \{\s*super.(?:<init>|\$init\$)(\([^)]*\))[^}]*};?""",
	  		"$1 $2$3$6 extends $4$7 $5")
	  	.replaceAll("""(?m)(?s)(class|trait|object) (\w+)(\[\w+(?:, \w+)*\])? ([^\n{]*?\{).*?def (?:<init>|\$init\$)(\([^)]*\)) = \{[^}]*};?""",
	  		"$1 $2$3$5 $4")
	  	.replaceAll("""\(\) (class|object|abstract trait)""", "\n  $1")
	  	.replaceAll("abstract trait", "trait")
	  	.replaceAll("""= \$qmark\$qmark\$qmark;?""", "= ???")
	  	.replaceAll("""= _;""", "= _")
	  	.replaceAll("""(trait|class|object) (\w+)(\[\w+(?:, \w+)*\])?\(\) """, "$1 $2$3 ")
	  	.replaceAll("""\b(def|va[rl]) (_\w+)\b""", "$1 `$2`")
	  	// .replaceAll(": Unit = ???;", "")
	  	// .replaceAll("""(?m)(?s)def <init>\(\) = \{.*?\};""", "")
	  	// .replaceAll("""(?m)(?s)def \$init\$\(\) = \{.*?\};""", "")
	  	.replaceAll("""(?<=\w)\$(?!\w)""", "") // Constructor params...
	  	.replaceAll("""[:,] (Array)\[""", """: scala.$1[""")
	  	.replaceAll("""\bOption\[""", """scala.Option[""")
	}
	def generate(sources: List[SourceFile], ownerName: String = "js", filter: Scope.Var => Boolean = null): String = {

		import scala.reflect.runtime.{ universe => ru }
	  val generator = new TreesGenerator(ru)

	  val sigs = generator.generateSignatures[ru.Tree](sources, ownerName, filter)
	  // println(sigs)

	  val src = postProcessPrintedTree(sigs.toString)
	  // println(src)
	  src
	}

	{
		val src = generate(ClosureCompilerUtils.defaultExterns :+ SpecialCases.missingConstructors)
		import java.io._
		val f = new File("../../out.scala")
		val out = new PrintStream(f)
		out.println(src)
		out.close()
		println("WROTE " + f)
		System.exit(0)
	  // println(src)
	}
}



  // for (variable <- scope.getVars) {
  //   println(variable.getName + ": " + variable.getType)// + Option(variable.getInitialValue).map(" = " + _).getOrElse(""))
  //   for (doc <- Option(variable.getJSDocInfo)) {
  //     val params = for (paramName <- doc.getParameterNames) yield {
  //       paramName -> getType(doc.getParameterType(paramName))
  //     }
  //     println(s"""
  //       type = ${getType(doc.getType)}
  //       extended interfaces = ${doc.getExtendedInterfaces.map(getType(_)).mkString(", ")}
  //       implemented interfaces = ${doc.getImplementedInterfaces.map(getType(_)).mkString(", ")}
  //       base type = ${doc.getBaseType}
  //       this type = ${doc.getThisType}
  //       params = ${params.mkString(", ")}
  //       return type = ${doc.getReturnType}
  //     """)
  //   }
  // }
  // println(compiler.toSource())
  //val globalVars = compiler.getGlobalVarReferences
  //println(globalVars)
  //println(compiler.getTopScope)
  /*
  for (jsdoc <- symTab.getAllJSDocInfo()) {
    println(jsdoc.getName)
  }
  for (variable <- globalScope.getVars) {
     println(variable.getName + ": " + variable.getType)
  }
  */

  /*
  compiler.setPassConfig(new PassConfig.PassConfigDelegate(new DefaultPassConfig(options)) {

    override def getChecks = {
      println("GOT CHECKS")
      val checks = super.getChecks
      checks.add(new PassFactory("externsHarvesting", true) {
        override def create(compiler: AbstractCompiler) = {
          new CompilerPass {
            override def process(externs: Node, root: Node) {
              // println("externs = " + externs.toStringTree) 
              // println(compiler.getTopScope) 
              // println(compiler.getTypeRegistry)
              printTypes(compiler.asInstanceOf[Compiler])
              // val ns =
              //   compiler
              //   .asInstanceOf[Compiler]
              //   .getPassConfig
              //   .getBasePassConfig
              //   .asInstanceOf[DefaultPassConfig]
              //   .getGlobalNamespace
              // println(s"ns in checks = $ns")
              //println(compiler.asInstanceOf[Compiler].buildKnownSymbolTable())
            }
          }
        }
      })
      checks
    }
  })
  */
  // See CommandLineRunner.getDefaultExterns()

    // val symTab = compiler.buildKnownSymbolTable()
    // val globalScope = symTab.getGlobalScope
    // for (sym <- symTab.getAllSymbols) {
    //   println(sym.getName + ": " + sym.getFunctionType)
    //   for (doc <- Option(sym.getJSDocInfo)) {
    //     println(s"\ttype = ${doc.getType}")
    //     println(s"\textended interfaces = ${doc.getExtendedInterfaces}")
    //     println(s"\timplemented interfaces = ${doc.getImplementedInterfaces}")
    //     println(s"\tbase type = ${doc.getBaseType}")
    //     println(s"\tthis type = ${doc.getThisType}")
    //     val params = for (paramName <- doc.getParameterNames) yield {
    //       paramName -> doc.getParameterType(paramName)
    //     }
    //     println(s"""\tparams = ${params.mkString(", ")}""")
    //     println(s"\treturn type = ${doc.getReturnType}")
    //   }
    // }

    // val ns = compiler.getPassConfig.getBasePassConfig.asInstanceOf[DefaultPassConfig].getGlobalNamespace
    // println(s"ns = $ns")
    // val scope = compiler.getTopScope
    // println(s"top scope = $scope")

