package scalaxy.js

import com.google.javascript.jscomp.ClosureCompilerUtils
import com.google.javascript.jscomp._

import scala.collection.JavaConversions._

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

class JavaScriptExterns(paths: String*) extends StaticAnnotation {
  // def macroTransform(annottees: Any*) = macro JavaScriptExterns.implementation
}

object JavaScriptExterns {
  def implementation(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val q"new JavaScriptExterns(..$pathTrees)" = c.prefix.tree
    val paths = pathTrees.map {
      case Literal(Constant(path: String)) => path
    }
    println("PATHS:\n\t" + paths.mkString(",\n\t"))


    val generator = new TreesGenerator(c.universe)

    val trees = annottees.map(_.tree).toList match {
      case List(q"object $name extends scala.AnyRef { ..$existingDecls }") =>

        // println("PATHS:\n\t" + paths.mkString(",\n\t"))
        val allExterns = SpecialCases.missingConstructors :: ClosureCompilerUtils.defaultExterns

        generator.generateSignatures[Tree](
      		allExterns,
      		name.toString,
      		extraDecls = existingDecls) :: Nil

      case _ =>
        c.error(c.enclosingPosition, "This annotation can only be set on an object, found on: " + annottees.map(_.tree))
        Nil
    }

    write(trees.mkString("\n"), new java.io.File("out.scala"))

    // println("OUT =\n" + trees.mkString("\n"))
    c.Expr[Any](Block(trees, c.literalUnit.tree))
  }
}
