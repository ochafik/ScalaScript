package scalaxy.js

import scala.collection.JavaConversions._
import scala.reflect.api.Universe
import com.google.javascript.jscomp._
import com.google.javascript.rhino._
import com.google.javascript.rhino.jstype._

case class FunctionTypeInfo(
		params: List[(String, JSType)],
		returnType: JSType,
		templateParams: List[String],
		thisTemplateParams: Option[List[String]]) {

	import ExtractorUtils._

	private def best[T](a: T, b: T)(isEmpty: T => Boolean)(default: => T): T =
		if (isEmpty(a)) b
		else if (isEmpty(b)) a
		else default

	def ++(info: FunctionTypeInfo) = {
		println("THIS.params = " + params)
		println("INFO.params = " + info.params)
		FunctionTypeInfo(
			params = best(params, info.params)(_.isEmpty) {
				assert(params.size == info.params.size)
				for (((n1, t1), (n2, t2)) <- params.zip(info.params)) yield {
					best(n1, n2)(stringIsEmpty)(n1) -> best(t1, t2)(typeIsEmpty)(t1)
				}
			},
			returnType = best(returnType, info.returnType)(typeIsEmpty)(returnType),
			templateParams = best(templateParams, info.templateParams)(_.isEmpty)(templateParams),
			thisTemplateParams = best(thisTemplateParams, info.thisTemplateParams)(_ == None)(thisTemplateParams)
		)
	}

	def withNamedParams =
		copy(params = for (((n, t), i) <- params.zipWithIndex) yield {
			(if (n.trim.isEmpty) "param" + i else n) -> t
	  })
}

object ExtractorUtils {
  val typeIsEmpty = (t: JSType) => t == null || t.isInstanceOf[NoType] || t.isInstanceOf[UnknownType]
  val stringIsEmpty = (s: String) => s.isEmpty

  private object WithDisplayName {
  	def unapply(t: JSType) = Option(t.getDisplayName)
  }
  def getTemplateParamsForThisType(doc: JSDocInfo)
  																(implicit compiler: ClosureCompiler)
  																: Option[List[String]] = {
  	def getTemplateTypes(t: JSType): Option[List[JSType]] = {
	  	t match {
	  		case t: UnionType =>
	  			def alt(alts: List[JSType]): Option[List[JSType]] = alts match {
	  				case Nil => None
	  				case x :: xs =>
	  					getTemplateTypes(x).orElse(alt(xs))
	  			}
	  			alt(t.getAlternates.toList)
	  		//case t: TemplatizedType =>
	  		case t: ObjectType =>
	  			println("T = " + t)
	  			val r = Option(t.getTemplateTypes).map(_.toList)
	  			println("\tr = " + r)
	  			r
	  		case _ =>
	  			// println(t.getClass.getName)
	  			None
	  	}
	  }

	  import compiler._
  	val tparamsOpt = getTemplateTypes(doc.getThisType)
	  val templateParams = doc.getTemplateTypeNames.toSet
  	for (tparams <- tparamsOpt) yield {
	  	(tparams collect {
				case t: UnionType =>
					t.getAlternates.toList collect {
						case WithDisplayName(n) if templateParams.contains(n) => n
					}
				case WithDisplayName(n) if templateParams.contains(n) => 
					List(n)
			}).flatten
	  }
  }
}

object SomeFunctionTypeInfo {
	import ExtractorUtils._

	def getFunctionInfo(doc: JSDocInfo)(implicit compiler: ClosureCompiler) = {
		import compiler._
		FunctionTypeInfo(
			params = for (n <- doc.getParameterNames.toList) yield {
				val t = doc.getParameterType(n)
	  		n -> (t: JSType)
	    },
	    returnType = doc.getReturnType,
	    templateParams = doc.getTemplateTypeNames.toList,
	    thisTemplateParams = getTemplateParamsForThisType(doc))
	}

	def getFunctionInfo(tpe: FunctionType)(implicit compiler: ClosureCompiler) = {
		import compiler._
		FunctionTypeInfo(
			params = for (p <- tpe.getParameters.toList) yield {
	      p.getString -> p.getJSType
	    },
	    returnType = tpe.getReturnType,
	    templateParams = Nil,
	    thisTemplateParams = None)
	}

	def unapply(tpe: JSType)
						 (implicit compiler: ClosureCompiler)
						 : Option[FunctionTypeInfo] =
		Option(tpe) collect {
			case ft: FunctionType =>
				getFunctionInfo(ft).withNamedParams
		}

  //private def children(n: Node) = (0 until n.getChildCount).map(n.getChildAtIndex(_)).toList
  private def getFunctionParamNames(n: Node): Option[List[String]] = {
  	if (!n.isFunction)
  		None
		else
			n.children.find(_.isParamList).map(n => {
				val cs = n.children
				// Hack: n.toString gives:
				//   NAME x 9 [source_file: test.js] : T
				// But n.getString gives:
				//   x$$1
				// So we cheat and remove anything after $
				cs.toList.map(_.getString.replaceAll("""\$.*""", ""))
			})
  }

	def getFunctionInfo(n: Node): Option[FunctionTypeInfo] = {
		for (names <- getFunctionParamNames(n)) yield {
			FunctionTypeInfo(
				params = for (n <- names) yield n -> (null: JSType),
		    returnType = null,
		    templateParams = Nil,
		    thisTemplateParams = None)
		}
	}

	def unapply(v: Scope.Var)
						 (implicit compiler: ClosureCompiler)
						 : Option[FunctionTypeInfo] = {
	 	Option(v.getType) collect {
	 		case tpe: FunctionType =>
	 			val tpeInfo = Some(getFunctionInfo(tpe))
	 			val docInfo = Option(v.getJSDocInfo).map(getFunctionInfo(_))
	 			val nodeInfo = Option(v.getInitialValue).flatMap(getFunctionInfo(_))

	 			Seq(tpeInfo, docInfo, nodeInfo)
	 				.flatten
	 				.reduceLeft(_ ++ _)
	 				.withNamedParams
	 	}
	}
}
