package scalaxy.js

import scala.collection.JavaConversions._
import scala.reflect.api.Universe
import com.google.javascript.jscomp._
import com.google.javascript.rhino._
import com.google.javascript.rhino.jstype._

class TreesGenerator(val global: Universe) {

  import global._

  import SpecialCases._

  def generateSignatures[T <: Universe#Tree](
  		sources: List[SourceFile],
  		ownerName: String,
  		filter: Scope.Var => Boolean = null,
  		extraDecls: List[T] = Nil)
  	: T = {

  	val packageName: TermName = ownerName
  	implicit val externs = ClosureCompilerUtils.scanExterns(sources)
    implicit val resolver = Resolver((simpleName: Name) => {
      Select(Ident(packageName), simpleName)
    })
    val globalVars = ExternsAnalysis.analyze(externs, filter)

    val generatedDecls: List[Tree] = globalVars.classes.flatMap(classVars => {
      generateClass(classVars, packageName)
    })
    val generatedGlobals: List[Tree] = globalVars
    	.globalVars
    	.map(convertMember(_))
    	.map(fixQuasiquotesInitSuperCall)

   val allDecls = generatedDecls ++ generatedGlobals
    q"""
      object $packageName extends scala.AnyRef {
        ..$allDecls
      }
    """.asInstanceOf[T]
  }

  case class Resolver(f: Name => Tree) {
  	def apply(n: Name) = f(n)
  }

  private def getMods(jsType: JSType): Modifiers = {
  	val optDocInfo = Option(jsType).flatMap(t => Option(t.getJSDocInfo))
  	val mutable = !optDocInfo.exists(_.isConstant)
		val deprecated = optDocInfo.exists(_.isDeprecated)

		var mods = if (mutable) Modifiers(Flag.MUTABLE) else NoMods
		if (deprecated) {
			mods = NoMods.mapAnnotations(list => q"new scala.deprecated" :: list)
		}
  	mods
  }

  def optional(isOptional: Boolean, tpt: Tree): Tree = {
	  if (isOptional)
	    AppliedTypeTree(
	      Ident(rootMirror.staticClass("scala.Option")),
	      List(tpt))
	  else
	    tpt
  }

  def functionTypeTree(paramTpts: List[Tree], returnTpt: Tree): Tree =
  	AppliedTypeTree(
      Ident(
      	rootMirror.staticClass(
      		"scala.Function" +
      		(if (paramTpts.isEmpty) "" else paramTpts.size))),
      paramTpts :+ returnTpt)

  def arrayTypeTree(elementType: Tree)(implicit resolver: Resolver): Tree =
  	AppliedTypeTree(
  		// Ident(rootMirror.staticClass("scala.Array")),
    	resolver("Array": TypeName),
    	List(elementType))

  private def convertTypeRef(jsType: JSType,
  													 templateTypeNames: Set[String],
  													 nullable: Boolean = false)
  									(implicit externs: ClosureCompiler, resolver: Resolver)
  									: (Modifiers, Tree) = {
  	import externs._

  	def default = TypeTree(if (nullable) typeOf[AnyRef] else typeOf[Any])
    // println(s"jsType = $jsType (${Option(jsType).map(_.getClass.getName)}, jsDocInfo = ${Option(jsType).map(_.getJSDocInfo).orNull}")
  	def convertType(t: JSType): Tree = t match {
  	  case null =>
      	default

      case t if templateTypeNames(t.toString) =>
      	default

      case (_: VoidType) | (_: NoType) =>
        TypeTree(typeOf[Unit])

      case (_: AllType) | (_: UnknownType) => //  | (_: NoType)
      	(
      		for (jsDocInfo <- Option(t.getJSDocInfo))
    			yield convertType(jsDocInfo.getType)
  			).getOrElse(default)

      case t: NullType =>
        TypeTree(typeOf[Null])

      case t: BooleanType =>
      	TypeTree(if (nullable) typeOf[java.lang.Boolean] else typeOf[Boolean])
        // optional(nullable, TypeTree(typeOf[Boolean]))

      case t: NumberType =>
      	TypeTree(if (nullable) typeOf[java.lang.Double] else typeOf[Double])
        // optional(nullable, TypeTree(typeOf[Double]))

      case t: StringType =>
        optional(nullable, TypeTree(typeOf[String]))

      case t: UnionType =>
        var hasNull = false
        var hasUndefined = false
        val alts = t.getAlternates.toList.filter(t => t match {
          case _: NullType =>
            hasNull = true
            false
          case _: VoidType =>
            hasUndefined = true
            false
          case _ =>
            true
        })
        val convertedAlts =
        	alts.map(t => convertTypeRef(t, templateTypeNames, nullable = hasNull)._2)

        val conv = convertedAlts match {
          case List(t) =>
            t
          case ts =>
            // println("Reducing types: " + ts.mkString(", ") + " (jsType = " + jsType + ")")
            ts.reduceLeft((a, b) => {
              AppliedTypeTree(
                //Ident(rootMirror.staticClass("scala.util.Either")),
                Ident(rootMirror.staticClass("scala.util.Either")),
                // TODO: alternatives in separate dependency
                // Ident(rootMirror.staticClass("scalaxy.js.|")),
                List(a, b)
              )
            })
        }
        optional(hasUndefined, conv)

      case t: FunctionType =>
      	val parameters = t.parameters
      	val returnType = convertTypeRef(t.getReturnType, templateTypeNames)._2

      	val actualParams =
        	if (parameters.isEmpty) List(TypeTree(typeOf[Array[Any]]))
        	else parameters.map(p => convertTypeRef(p._2, templateTypeNames)._2)

        optional(nullable, functionTypeTree(actualParams, returnType))

      case t: ObjectType =>
        val n = t.getDisplayName

        {
        	// println(s"t.getTemplateTypes = ${t.getTemplateTypes}")
        	// println(s"t.parameters = ${t.parameters})")
          // println(s"t.getJSDocInfo.parameters = ${t.getJSDocInfo.parameters})")
          (n, Option(t.getTemplateTypes).map(_.toList).getOrElse(Nil)) match {
            case ("Array", elementTypes) =>
            	arrayTypeTree(
            		elementTypes match {
	            		case List(elementType) =>
	            			convertTypeRef(elementType, templateTypeNames)._2
	          			case Nil =>
	          				TypeTree(typeOf[Any])
	      				})
            case ("Object", _) =>
              TypeTree(typeOf[AnyRef])
            case (_, Nil) =>
              Option(resolver).map(_(n: TypeName)).getOrElse {
                Ident(n: TypeName)
              }
            case _ =>
              sys.error("Template type not handled for type " + n + ": " + jsType)
          }
        }
    }

    getMods(jsType) -> convertType(jsType)
  }

  def defaultValue(tpe: Type): Any = {
  	import definitions._

  	tpe.normalize match {
	    case IntTpe => 0
	    case BooleanTpe => false
	    case ByteTpe => 0: Byte
	    case ShortTpe => 0: Short
	    case CharTpe => '\0'
	    case LongTpe => 0L
	    case FloatTpe => 0.0f
	    case DoubleTpe => 0.0
	    case s => null
	  }
	}

  def getParams(info: FunctionTypeInfo, rename: Boolean = false)
  						 (implicit externs: ClosureCompiler, resolver: Resolver) = {
  	for ((name, tpe) <- info.params) yield
  		ValDef(
				NoMods,
				(if (rename) name + "$" else name): TermName,
				convertTypeRef(tpe, info.templateParamsSet)._2,
				EmptyTree)
  }

  def convertMember(memberVar: Scope.Var)
  								 (implicit externs: ClosureCompiler, resolver: Resolver): Tree = {
    val memberName: TermName = memberVar.getName.split("\\.").last
    val templateTypeNames = Option(memberVar.getJSDocInfo).map(_.getTemplateTypeNames().toSet).getOrElse(Set())
    if (!templateTypeNames.isEmpty)
      EmptyTree
    else if (memberName.toString == "toString")
      EmptyTree
    else {
      assert(memberName.toString.trim != "")
      memberVar.getType match {
        case ft: FunctionType =>
        	val Some(info) = SomeFunctionTypeInfo.unapply(memberVar)
        	var mods =
            if (info.isOverride && !invalidOverrideExceptions(memberVar.getName) ||
                missingOverrideExceptions(memberVar.getName))
              Modifiers(Flag.OVERRIDE)
            else
              NoMods
          if (info.isDeprecated)
          	mods = NoMods.mapAnnotations(list => q"new scala.deprecated" :: list)
          DefDef(
          	mods,
          	memberName,
          	Nil,
          	List(getParams(info)),
          	convertTypeRef(info.returnType, info.templateParamsSet)._2,
          	q"???")
        case t =>
          val (mods, valType) = Option(t).map(convertTypeRef(_, templateTypeNames)).getOrElse(NoMods -> TypeTree(typeOf[Any]))
          val vd = ValDef(mods, memberName, valType,
        		if (mods.hasFlag(Flag.MUTABLE)) EmptyTree
        		else Literal(Constant(defaultValue(valType.tpe))))
          // println("vd = " + vd + " (valType.tpe = " + valType.tpe + ")")
          vd
          // q"var $memberName: $valType = _"
      }
    }
  }

  def generateClass(classVars: ClassVars, owner: Name)
  								 (implicit externs: ClosureCompiler, resolver: Resolver)
  								 : List[Tree] = {
    import externs._

    val (fullClassName, packageName, simpleClassName) = classVars.className match {
      case fullClassName @ qualNameRx(packageName, simpleClassName) =>
        (fullClassName, packageName, simpleClassName)
      case className =>
        (className, null, className)
    }

    

    val constructorInfo = classVars.constructor.flatMap(SomeFunctionTypeInfo.unapply(_))

    val className = simpleClassName: TypeName
    val companionName = simpleClassName: TermName


    val protoMembers = classVars.protoMembers.map(convertMember(_))
    val staticMembers = classVars.staticMembers.filter(!_.getName.endsWith(".prototype")).map(convertMember(_))

    val companion =
      if (staticMembers.isEmpty)
        Nil
      else
        //def apply(..${getParams(classDoc)}): $className = ???
        q"""
          @scalaxy.js.global
          object $companionName {
            ..$staticMembers
          }
        """ :: Nil

    lazy val traitTree =
      fixQuasiquotesInitSuperCall(q"""
        @scalaxy.js.global
        trait $className {
          ..$protoMembers
        }
      """)

    val result = (constructorInfo, constructorInfo.flatMap(_.doc)) match {
      case (None, None) =>
        traitTree :: companion
      case _ if classVars.constructor.get.getType.isInterface =>
        traitTree :: companion
      case (Some(info), Some(doc)) =>
        // TODO add template params from methods (e.g. for Array)
        val templateTypeNames = info.templateParamsSet

        val parents = {
          val interfaces =
            (doc.getExtendedInterfaces.toList ++ doc.getImplementedInterfaces.toList)
            .toSet.toList.map((t: JSTypeExpression) =>  convertTypeRef(t, templateTypeNames)._2)
          if (interfaces.isEmpty) {
            if (simpleClassName == "Object" || simpleClassName == "Number" || simpleClassName == "Boolean")
              List(TypeTree(typeOf[AnyRef]))
            else
              List(resolver("Object": TypeName))
            // List(TypeTree(typeOf[AnyRef]))
          } else {
            interfaces
          }
        }

        val cparams = getParams(info, rename = true)
        val classDef = q"""
          @scalaxy.js.global
          class $className(..$cparams)
              extends ..$parents {
            ..$protoMembers
          }
        """
        fixQuasiquotesInitSuperCall(classDef) :: companion
      // }
    }
    result.map(fixQuasiquotesInitSuperCall)
  }

  val fixQuasiquotesInitSuperCall = {
    /*
      Quasiquotes generate a constructor like this:

        def <init>() = {
          super.<init>;
          ()
        };

      This transformer will simply add the () to get super.<init>(); 
    */
    val fixer = new Transformer {
      override def transform(tree: Tree) = tree match {
        case Block(List(Select(target, nme.CONSTRUCTOR)), value) =>
          Block(List(Apply(Select(target, nme.CONSTRUCTOR), Nil)), value)
        case ClassDef(mods, name, tparams, Template(parents, self, body)) if mods.hasFlag(Flag.TRAIT) =>
          // println("FOUND TRAIT")
          ClassDef(
            mods,
            name,
            tparams,
            Template(
              parents,
              self,
              body.filter({
                case d: DefDef if d.name == nme.CONSTRUCTOR => false
                case _ => true
              })
            )
          )
        case _ =>
          super.transform(tree)
      }
    } 
    (tree: Tree) => fixer.transform(tree)
  }

  def generateGlobal(u: Universe)(variable: Scope.Var): List[Tree] = {
    Nil
  }
}
