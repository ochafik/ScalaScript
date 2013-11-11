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

    // println("\n\nGLOBAL VARS:\n" + globalVars + "\n\n")

    val generatedDecls: List[Tree] = globalVars.classes.flatMap(classVars => {
      generateClass(classVars, packageName)
    })
    val generatedGlobals: List[Tree] = globalVars
    	.globalVars
    	.filterNot(_.getName.contains(".")) // TODO: namespaces
    	.flatMap(v =>
    		try { Some(convertMember(v)._1) }
    		catch { case ex: Throwable => None })
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

  case class ConvertedType(mods: Modifiers, tpt: Tree, isOptional: Boolean = false)

  private def convertTypeRef(jsType: JSType,
  													 templateTypeNames: Set[String],
  													 nullable: Boolean = false)
				  									(implicit externs: ClosureCompiler, resolver: Resolver)
				  									: ConvertedType = {
  	import externs._

  	val mods = getMods(jsType)
  	def conv(tpt: Tree, isOptional: Boolean = false) =
  		ConvertedType(mods, tpt, isOptional = isOptional)

  	def default = conv(TypeTree(if (nullable) typeOf[AnyRef] else typeOf[Any]))
    // println(s"jsType = $jsType (${Option(jsType).map(_.getClass.getName)}, jsDocInfo = ${Option(jsType).map(_.getJSDocInfo).orNull}")

  	def convertType(t: JSType): ConvertedType = t match {
  	  case null =>
      	default

      case t if templateTypeNames(t.toString) =>
      	conv(Ident(t.toString: TypeName))

      case (_: VoidType) | (_: NoType) =>
        conv(TypeTree(typeOf[Unit]))

      case (_: AllType) | (_: UnknownType) => //  | (_: NoType)
      	(
      		for (jsDocInfo <- Option(t.getJSDocInfo))
    			yield convertType(jsDocInfo.getType)
  			).getOrElse(default)

      case t: NullType =>
        conv(TypeTree(typeOf[Null]))

      case t: BooleanType =>
      	conv(TypeTree(if (nullable) typeOf[java.lang.Boolean] else typeOf[Boolean]))

      case t: NumberType =>
      	conv(TypeTree(if (nullable) typeOf[java.lang.Double] else typeOf[Double]))

      case t: StringType =>
        conv(TypeTree(typeOf[String]))

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
        	alts.map(t => convertTypeRef(t, templateTypeNames, nullable = hasNull).tpt)

        val union = convertedAlts match {
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

        conv(optional(hasUndefined, union), hasUndefined)

      case t: FunctionType =>
      	val parameters = t.parameters
      	val returnType = convertTypeRef(t.getReturnType, templateTypeNames).tpt

      	val actualParams =
        	if (parameters.isEmpty) List(TypeTree(typeOf[Array[Any]]))
        	else parameters.map(p => convertTypeRef(p._2, templateTypeNames).tpt)

        conv(optional(nullable, functionTypeTree(actualParams, returnType)), nullable)

      case t: ObjectType =>
        val n = t.getDisplayName

        {
        	// println(s"t.getTemplateTypes = ${t.getTemplateTypes}")
        	// println(s"t.parameters = ${t.parameters})")
          // println(s"t.getJSDocInfo.parameters = ${t.getJSDocInfo.parameters})")
          conv((n, Option(t.getTemplateTypes).map(_.toList).getOrElse(Nil)) match {
            case ("Array", elementTypes) =>
            	arrayTypeTree(
            		elementTypes match {
	            		case List(elementType) =>
	            			convertTypeRef(elementType, templateTypeNames).tpt
	          			case Nil =>
	          				TypeTree(typeOf[Any])
	      				})
            case ("Object", _) =>
              TypeTree(typeOf[AnyRef])
            case (_, Nil) if n != null =>
              Option(resolver).map(_(n: TypeName)).getOrElse {
                Ident(n: TypeName)
              }
            case _ =>
              sys.error("Template type not handled for type " + n + ": " + jsType + " (: " + jsType.getClass.getName + "; isRecordType = " + jsType.isRecordType + ")")
          })
        }
    }

    convertType(jsType)
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
  	for ((name, jsType) <- info.params) yield {
  		val conv = convertTypeRef(jsType, info.templateParamsSet)
  		ValDef(
				NoMods,
				(if (rename) name + "$" else name): TermName,
				conv.tpt,
				if (conv.isOptional) q"None" else EmptyTree)
  	}
  }

  def convertMember(memberVar: Scope.Var)
  								 (implicit externs: ClosureCompiler, resolver: Resolver): (Tree, Option[FunctionTypeInfo]) = {
    val memberName: TermName = memberVar.getName.split("\\.").last
    val templateTypeNames = Option(memberVar.getJSDocInfo).map(_.getTemplateTypeNames().toSet).getOrElse(Set())

    if (memberName.toString == "toString")
      EmptyTree -> None
    else {
      assert(memberName.toString.trim != "")
      memberVar.getType match {
        case ft: FunctionType =>
        	val optInfo @ Some(info) = SomeFunctionTypeInfo.unapply(memberVar)
        	var mods =
            if (info.isOverride && !invalidOverrideExceptions(memberVar.getName) ||
                missingOverrideExceptions(memberVar.getName))
              Modifiers(Flag.OVERRIDE)
            else
              NoMods
          if (info.isDeprecated)
          	mods = NoMods.mapAnnotations(list => q"new scala.deprecated" :: list)

          val thisTemplateParamsSet = info.thisTemplateParams.getOrElse(Nil).toSet

          DefDef(
          	mods,
          	memberName,
          	info.templateParams.filterNot(thisTemplateParamsSet).map(templateTypeDef(_)),
          	List(getParams(info)),
          	convertTypeRef(info.returnType, info.templateParamsSet).tpt,
          	q"???") -> optInfo

        case t =>
          val conv = Option(t).map(convertTypeRef(_, templateTypeNames))
          	.getOrElse(ConvertedType(NoMods, TypeTree(typeOf[Any])))
          val vd = ValDef(conv.mods, memberName, conv.tpt,
        		if (conv.mods.hasFlag(Flag.MUTABLE)) EmptyTree
        		else Literal(Constant(defaultValue(conv.tpt.tpe))))

          vd -> None
      }
    }
  }

  private val scalaNothing = rootMirror.staticClass("scala.Nothing")

  private val scalaAny = rootMirror.staticClass("scala.Any")

  private def templateTypeDef(n: TypeName) =
		TypeDef(
			Modifiers(Flag.PARAM),
			n: TypeName, Nil,
			TypeBoundsTree(Ident(scalaNothing), Ident(scalaAny))
		)

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

    val constructorInfo: Option[FunctionTypeInfo] =
    	classVars.constructor.flatMap(SomeFunctionTypeInfo.unapply(_))

    val className: TypeName = simpleClassName
    val companionName: TermName = simpleClassName

    val (protoMembers, functionInfos: List[Option[FunctionTypeInfo]]) =
    	classVars.protoMembers.map(convertMember(_)).unzip
    val staticMembers = classVars.staticMembers.filter(!_.getName.endsWith(".prototype")).map(convertMember(_)._1)

    // println("PROTO MEMBERS:\n\t" + protoMembers.mkString("\n\t"))
    // println("FUNCTION INFOS:\n\t" + functionInfos.mkString("\n\t"))

    val tparamDefs = {
    	val inferredTemplateParams: List[List[String]] =
    		functionInfos.flatten.flatMap(_.thisTemplateParams)
    	val set: Set[List[String]] =
    		(inferredTemplateParams ++ constructorInfo.map(_.templateParams))
    			.filterNot(_.isEmpty).toSet
    	set.toList match {
    		case Nil => Nil
    		case List(tparams) => tparams.map(templateTypeDef(_))
    		case _ =>
    			sys.error("Found mismatching template params for this type " + className + ": " + set)
    	}
    }

    // println("TPARAM DEFS FOR " + className + ": " + tparamDefs)
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
            .toSet.toList.map((t: JSTypeExpression) =>  convertTypeRef(t, templateTypeNames).tpt)
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
          class $className[..$tparamDefs](..$cparams)
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
