package com.google.javascript.jscomp

import scalaxy.js._

import scala.collection.JavaConversions._

import com.google.javascript.rhino.Node
import com.google.javascript.rhino.JSTypeExpression
import com.google.javascript.rhino.jstype._

object ClosureCompilerUtils {
  def defaultExterns: List[SourceFile] = CommandLineRunner.getDefaultExterns().toList

  /**
   * See node.js externs: https://github.com/dcodeIO/node.js-closure-compiler-externs
   * Other extends: https://code.google.com/p/closure-compiler/wiki/ExternsForCommonLibraries
   * And yet other ones: http://closureplease.com/externs/
   *
   * Automatic externs extractor:
   * http://blog.dotnetwise.com/2009/11/closure-compiler-externs-extractor.html
   */
  def scanExterns(externs: List[SourceFile] = defaultExterns): ClosureCompiler = {
    val code = "window.console.loge('yay');"

    val compiler = new Compiler

    val options = new CompilerOptions()
    options.checkTypes = true
    options.inferTypes = true

    //val externs = SourceFile.fromCode("externs.js", "") :: Nil
    // val externs = CommandLineRunner.getDefaultExterns()
    val inputs = java.util.Collections.singletonList(SourceFile.fromCode("input.js", code))

    // compile() returns a Result, but it is not needed here.
    compiler.compile(externs, inputs, options)

    val scopeCreator = new TypedScopeCreator(compiler)
    val scope = scopeCreator.createScope(compiler.getRoot, null)

    new ClosureCompiler(compiler, scope)
  }
}
