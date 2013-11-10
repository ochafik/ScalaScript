import sbt._
import Keys._
import sbtassembly.Plugin._ ; import AssemblyKeys._
import ls.Plugin._

import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform.scalariformSettings
import com.typesafe.sbt.SbtScalariform._
import com.typesafe.sbt.SbtGit.GitKeys._
import com.typesafe.sbt.SbtSite._
import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.SbtGhPages._

/*
Scalaxy/Reified:
  - Normal tests:
    sbt 'project scalaxy-reified' ~test

  - Remote-debug tests on port 5005:
    sbt 'project scalaxy-reified' 'set javaOptions in Test ++= Seq("-Xdebug", "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5005")' ~test

*/
object ScalaScriptBuild extends Build {
  // See https://github.com/mdr/scalariform
  ScalariformKeys.preferences := FormattingPreferences()
    .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
    .setPreference(PreserveDanglingCloseParenthesis, true)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentClassDeclaration, true)
    .setPreference(PreserveDanglingCloseParenthesis, false)

  lazy val scalaSettings = Seq(
    //exportJars := true, // use jars in classpath
    scalaVersion := "2.10.3"
    //scalaVersion := "2.11.0-M4",
    //scalaVersion := "2.11.0-SNAPSHOT",
    // crossScalaVersions := Seq("2.11.0-SNAPSHOT")
  )

  lazy val infoSettings = Seq(
    organization := "com.nativelibs4java",
    version := "0.3-SNAPSHOT",
    licenses := Seq("BSD-3-Clause" -> url("http://www.opensource.org/licenses/BSD-3-Clause")),
    homepage := Some(url("https://github.com/ochafik/ScalaScript")),
    gitRemoteRepo := "git@github.com:ochafik/ScalaScript.git",
    pomIncludeRepository := { _ => false },
    pomExtra := (
      <scm>
        <url>git@github.com:ochafik/ScalaScript.git</url>
        <connection>scm:git:git@github.com:ochafik/ScalaScript.git</connection>
      </scm>
      <developers>
        <developer>
          <id>ochafik</id>
          <name>Olivier Chafik</name>
          <url>http://ochafik.com/</url>
        </developer>
      </developers>
    ),
    (LsKeys.docsUrl in LsKeys.lsync) <<= homepage,
    (LsKeys.tags in LsKeys.lsync) :=
       Seq("compiler-plugin", "rewrite", "ast", "transform", "optimization", "optimisation"),
    (description in LsKeys.lsync) :=
      "A scalac compiler plugin that optimizes the code by rewriting for loops on ranges into while loops, avoiding some implicit object creations when using numerics...",
    LsKeys.ghUser := Some("ochafik"),
    LsKeys.ghRepo := Some("ScalaScript"))

  lazy val macroParadiseSettings =
    Seq(
      addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full)
    )

  lazy val standardSettings =
    Defaults.defaultSettings ++
    infoSettings ++
    sonatypeSettings ++
    seq(lsSettings: _*) ++
    Seq(
      javacOptions ++= Seq("-Xlint:unchecked"),
      scalacOptions ++= Seq(
        "-encoding", "UTF-8",
        // "-optimise",
        "-deprecation",
        "-Xlog-free-types",
        // "-Xlog-free-terms",
        // "-Yinfer-debug",
        //"-Xlog-implicits",
        //"-Ymacro-debug-lite", "-Ydebug",
        // "-Ymacro-debug-verbose",
        "-feature",
        "-unchecked"
      ),
      //scalacOptions in Test ++= Seq("-Xprint:typer"),
      //fork in Test := true,
      fork := true,
      parallelExecution in Test := false,
      libraryDependencies ++= Seq(
        "junit" % "junit" % "4.10" % "test",
        "com.novocode" % "junit-interface" % "0.8" % "test"
      )
    )

  lazy val reflectSettings =
    standardSettings ++
    scalaSettings ++
    Seq(
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)
    )

  lazy val sonatypeSettings = Seq(
    publishMavenStyle := true,
    resolvers += Resolver.sonatypeRepo("snapshots"),
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("-SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    })

  override lazy val settings =
    super.settings ++
    Seq(
      shellPrompt := { s => Project.extract(s).currentProject.id + "> " }
    ) ++ scalaSettings

  lazy val _scalaScript =
    Project(
      id = "scalascript",
      base = file("."),
      settings =
        standardSettings ++
        Seq(publish := { }))
    .aggregate(core, generator, compiler)

  lazy val core =
    Project(
    	id = "scalascript-core",
    	base = file("Core"),
    	settings = reflectSettings)

  lazy val generator =
    Project(
    	id = "scalascript-generator",
    	base = file("Generator"),
  		settings = reflectSettings ++ macroParadiseSettings ++ Seq(
	      libraryDependencies += "com.google.javascript" % "closure-compiler" % "v20130722"
	    )
    ).dependsOn(core)

  lazy val compiler =
    Project(
    	id = "scalascript-compiler",
    	base = file("Compiler"),
    	settings = reflectSettings ++ macroParadiseSettings
  	).dependsOn(core)

}
