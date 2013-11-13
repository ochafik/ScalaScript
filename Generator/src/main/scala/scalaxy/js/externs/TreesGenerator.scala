package scalaxy.js

import scala.util.Either
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
  	implicit val compiler = ClosureCompilerUtils.scanExterns(sources)
    implicit val resolver = Resolver((simpleName: Name) => {
      Select(Ident(packageName), simpleName)
    })
    val globalVars = ExternsAnalysis.analyze(compiler, filter)

    // println("\n\nGLOBAL VARS:\n" + globalVars + "\n\n")

    val generatedDecls: List[Tree] =
    	globalVars.classes.values.toList.sortBy(_.className).flatMap(generateClass(_, globalVars, packageName))

    // TODO: namespaces
    val generatedGlobals: List[Tree] = globalVars
    	.globalVars
    	.filterNot(v => {
    		val n = v.getName
    		n.contains(".") || n == "java"
  		})
    	.sortBy(_.getName)
    	.flatMap(v =>
    		try { Some(convertMember(v, Nil)._1) }
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

  private def getMods(jsType: JSType, isOverride: Boolean): Modifiers = {
  	val optDocInfo = Option(jsType).flatMap(t => Option(t.getJSDocInfo))
  	val mutable = !optDocInfo.exists(_.isConstant)
		val deprecated = optDocInfo.exists(_.isDeprecated)

		val flags = Seq(
			if (mutable) Some(Flag.MUTABLE) else None,
			if (isOverride) Some(Flag.OVERRIDE) else None
		).flatten

		var mods = if (flags.isEmpty) NoMods else Modifiers(flags.reduce(_ | _))
		if (deprecated) {
			mods = NoMods.mapAnnotations(list => q"new scala.deprecated" :: list)
		}
  	mods
  }

  

  def functionTypeTree(returnTpt: Tree, paramTpts: List[Tree]): Tree =
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

  def functionType(returnTpe: Type, paramTpes: List[Type]): Type =
  	appliedType(
  		rootMirror.staticClass(
  			"scala.Function" +
  			(if (paramTpes.isEmpty) "" else paramTpes.size)
			).asType.toType,
  		paramTpes :+ returnTpe)

  def arrayType(elementType: Type): Type =
  	appliedType(typeOf[Array[_]], List(elementType))

  def alternativeType(a: Type, b: Type): Type =
  	appliedType(typeOf[Either[_, _]], List(a, b))
              // Applied
              //   //Ident(rootMirror.staticClass("scala.util.Either")),
              //   Ident(rootMirror.staticClass("scala.util.Either")),
              //   // TODO: alternatives in separate dependency
              //   // Ident(rootMirror.staticClass("scalaxy.js.|")),
              //   List(a, b)
              // )

  case class ConvertedType(mods: Modifiers, tpe: Type, tpt: Tree, isOptional: Boolean = false)

  private def convertTypeRef(jsType: JSType,
		  											 templateTypeNames: Set[String],
		  											 nullable: Boolean = false,
		  											 isOverride: Boolean = false)
						  							(implicit externs: ClosureCompiler, resolver: Resolver)
						  						  : ConvertedType = {
  	import externs._

  	val mods = getMods(jsType, isOverride)
  	def conv(tpe: Type, tpt: Tree = null, isOptional: Boolean = false): ConvertedType =
  		ConvertedType(mods, tpe = tpe, tpt = Option(tpt).getOrElse(TypeTree(tpe)), isOptional = isOptional)

  	def default = conv(if (nullable) typeOf[AnyRef] else typeOf[Any])

  	def optional(isOptional: Boolean, c: ConvertedType) = {
		  if (isOptional)
		  	ConvertedType(
	  			mods,
	  			tpe = appliedType(typeOf[Option[_]], List(c.tpe)),
	  			tpt = AppliedTypeTree(
			      Ident(rootMirror.staticClass("scala.Option")),
			      List(c.tpt)),
	  			isOptional = isOptional)
		  else
		    c
	  }
	  def function(ret: ConvertedType, args: List[ConvertedType]) =
	  	ConvertedType(
  			mods,
  			tpe = functionType(ret.tpe, args.map(_.tpe)),
  			tpt = functionTypeTree(ret.tpt, args.map(_.tpt)))

	  def array(elementType: ConvertedType) =
	  	ConvertedType(
  			mods,
  			tpe = arrayType(elementType.tpe),
  			tpt = arrayTypeTree(elementType.tpt))

  	def alternative(a: ConvertedType, b: ConvertedType) =
	  	ConvertedType(
  			mods,
  			tpe = appliedType(typeOf[Either[_, _]], List(a.tpe, b.tpe)),
  			tpt = AppliedTypeTree(
		      Ident(rootMirror.staticClass("scala.util.Either")),
		      List(a.tpt, b.tpt)))
    // println(s"jsType = $jsType (${Option(jsType).map(_.getClass.getName)}, jsDocInfo = ${Option(jsType).map(_.getJSDocInfo).orNull}")

  	def convertType(t: JSType): ConvertedType = t match {
  	  case null =>
      	default

      case t if templateTypeNames(t.toString) =>
      	conv(typeOf[Any], Ident(t.toString: TypeName))

      case (_: VoidType) | (_: NoType) =>
        conv(typeOf[Unit])

      case (_: AllType) | (_: UnknownType) => //  | (_: NoType)
      	(
      		for (jsDocInfo <- Option(t.getJSDocInfo))
    			yield convertType(jsDocInfo.getType)
  			).getOrElse(default)

      case t: NullType =>
        conv(typeOf[Null])

      case t: BooleanType =>
      	conv(if (nullable) typeOf[java.lang.Boolean] else typeOf[Boolean])

      case t: NumberType =>
      	conv(if (nullable) typeOf[java.lang.Double] else typeOf[Double])

      case t: StringType =>
        conv(typeOf[String])

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
        	alts.map(t => convertTypeRef(t, templateTypeNames, nullable = hasNull))

        val union: ConvertedType = convertedAlts match {
          case List(t) =>
            t
          case ts =>
            // println("Reducing types: " + ts.mkString(", ") + " (jsType = " + jsType + ")")
            ts.reduceLeft((a, b) => alternative(a, b))
        }

        optional(hasUndefined, union)

      case t: FunctionType =>
      	val parameters = t.parameters
      	val returnType = convertTypeRef(t.getReturnType, templateTypeNames)

      	val actualParams =
        	if (parameters.isEmpty) List(conv(typeOf[Array[Any]]))
        	else parameters.map(p => convertTypeRef(p._2, templateTypeNames))

        optional(nullable, function(returnType, actualParams))

      case t: ObjectType if t.getDisplayName == null && t.isRecordType =>
      	// TODO create refined type that matches record
      	// val symbols: List[Symbol] = 
      	// 	for (p <- t.getOwnPropertyNames.toList.sorted;
	      // 			 jsType = t.getPropertyType(p)) yield {
      	// 		val info = SomeFunctionTypeInfo.unapply(jsType)
      	// 			.getOrElse(FunctionTypeInfo(jsType = jsType))
	      // 		val t = MethodType(
	      // 			info.params.map(p => convertTypeRef(p._2, info.templateParamsSet).tpe.typeSymbol),
	      // 			convertTypeRef(info.returnType, info.templateParamsSet).tpe)
	      // 		println("FOUND PROP " + p + ": " + jsType + " -> " + t)
	      // 		t.typeSymbol
	      // 	}
      	// val ref = conv(RefinedType(Nil, newScopeWith(symbols: _*)))
      	// println("REFINED TYPE: " + ref)
      	// ref
      	val memberSigs: List[String] = 
      		for (p <- t.getOwnPropertyNames.toList.sorted;
	      			 jsType = t.getPropertyType(p)) yield {
      			val info = SomeFunctionTypeInfo.unapply(jsType)
      				.getOrElse(FunctionTypeInfo(jsType = jsType))

      			val retSig = convertTypeRef(info.returnType, info.templateParamsSet).tpt.toString
      		  var paramSigs = info.params.map {
      				case (n, t) => n + ": " + convertTypeRef(t, info.templateParamsSet).tpt.toString
    				} mkString (", ")
    				if (!paramSigs.isEmpty)
    					paramSigs = "(" + paramSigs + ")"

    				s"""def $p$paramSigs: $retSig"""
	      	}
      	val sig = memberSigs.mkString("{ ", "; ", " }")
  	  	conv(
  	  		typeOf[Any],
	  			Annotated(
  					q"""new deprecated("record", $sig)""",
	  				TypeTree(typeOf[Any])))
      	// default

      case t: ObjectType =>
        val n = t.getDisplayName

        {
        	// println(s"t.getTemplateTypes = ${t.getTemplateTypes}")
        	// println(s"t.parameters = ${t.parameters})")
          // println(s"t.getJSDocInfo.parameters = ${t.getJSDocInfo.parameters})")
          (n, Option(t.getTemplateTypes).map(_.toList).getOrElse(Nil)) match {
            case ("Array", elementTypes) =>
            	array(
            		elementTypes match {
	            		case List(elementType) =>
	            			convertTypeRef(elementType, templateTypeNames)
	          			case Nil =>
	          				conv(typeOf[Any])
	      				})

            case ("Object", _) =>
              conv(typeOf[AnyRef])

            case (_, Nil) if n != null =>
              conv(typeOf[Any], Option(resolver).map(_(n: TypeName)).getOrElse {
                Ident(n: TypeName)
              })

            case _ =>
              sys.error("Template type not handled for type " + n + ": " + jsType + " (: " + jsType.getClass.getName + "; isRecordType = " + jsType.isRecordType + ")")
          }
        }
    }

    convertType(jsType)
  }

  def defaultValue(tpe: Type): Any = {
  	import definitions._

  	Option(tpe).map(_.normalize).orNull match {
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
  						 (implicit compiler: ClosureCompiler, resolver: Resolver) = {
  	for ((name, jsType) <- info.params) yield {
  		val conv = convertTypeRef(jsType, info.templateParamsSet)
  		ValDef(
				NoMods,
				(if (rename) name + "$" else name): TermName,
				conv.tpt,
				if (conv.isOptional) q"None" else EmptyTree)
  	}
  }

  def convertMember(memberVar: Scope.Var, parentTypes: List[JSType])
  								 (implicit compiler: ClosureCompiler, resolver: Resolver): (Tree, Option[FunctionTypeInfo]) = {
    val memberName: TermName = memberVar.getName.split("\\.").last
    val templateTypeNames = Option(memberVar.getJSDocInfo).map(_.getTemplateTypeNames().toSet).getOrElse(Set())

    val isOverride = parentTypes.exists(_.hasProperty(memberName.toString))

    if (memberName.toString == "toString" || isOverride)
      EmptyTree -> None
    else {
      assert(memberName.toString.trim != "")
      memberVar.getType match {
      	case ft: FunctionType =>
        	val optInfo @ Some(info) = SomeFunctionTypeInfo.unapply(memberVar)
        	var mods =
            if ((info.isOverride || isOverride) && !invalidOverrideExceptions(memberVar.getName) ||
                missingOverrideExceptions(memberVar.getName))
              Modifiers(Flag.OVERRIDE)
            else
              NoMods
          if (info.isDeprecated)
          	mods = NoMods.mapAnnotations(list => q"new scala.deprecated" :: list)

          val thisTemplateParamsSet =
          	if (memberVar.getName.startsWith("Array.prototype.")) Set("T")
          	else info.thisTemplateParams.getOrElse(Nil).toSet

          DefDef(
          	mods,
          	memberName,
          	info.templateParams.filterNot(thisTemplateParamsSet).map(templateTypeDef(_)),
          	List(getParams(info)),
          	convertTypeRef(info.returnType, info.templateParamsSet).tpt,
          	q"???") -> optInfo

        case t if memberVar.getJSDocInfo != null && memberVar.getJSDocInfo.hasTypedefType() =>
        	import compiler._
        	TypeDef(NoMods, memberVar.getName: TypeName, Nil,
      			convertTypeRef(memberVar.getJSDocInfo.getTypedefType, Set()).tpt) -> None

        case t =>
          val conv = Option(t).map(convertTypeRef(_, templateTypeNames, isOverride = isOverride))
          	.getOrElse(ConvertedType(NoMods, typeOf[Any], TypeTree(typeOf[Any])))
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

  def generateClass(classVars: ClassVars, globalVars: GlobalVars, owner: Name)
  								 (implicit compiler: ClosureCompiler, resolver: Resolver)
  								 : List[Tree] = {
    import compiler._

    val (fullClassName, packageName, simpleClassName) = classVars.className match {
      case fullClassName @ qualNameRx(packageName, simpleClassName) =>
        (fullClassName, packageName, simpleClassName)
      case className =>
        (className, null, className)
    }

    val constructorInfo: Option[FunctionTypeInfo] =
    	classVars.constructor.flatMap(SomeFunctionTypeInfo.unapply(_))

    val baseTypeOpt: Option[JSType] =
    	for (c <- classVars.constructor;
    			 doc <- Option(c.getJSDocInfo);
    			 baseType <- Option(doc.getBaseType))
    	yield baseType

    val interfaceTypes: List[JSType] =
    	classVars
    		.constructor
    		.flatMap(c => Option(c.getJSDocInfo))
    		.toList
    		.flatMap(doc => doc.getExtendedInterfaces.toList ++ doc.getImplementedInterfaces)
    		.map(t => t: JSType)

  	val parentTypes = baseTypeOpt.toList ++ interfaceTypes

    val baseConstructorInfoOpt: Option[FunctionTypeInfo] =
    	for (b <- baseTypeOpt;
    			 n <- Option(b.getDisplayName);
    			 cls <- globalVars.classes.get(n);
    			 ctor <- cls.constructor;
    			 info <- SomeFunctionTypeInfo.unapply(ctor))
  		yield info

    val className: TypeName = simpleClassName
    val companionName: TermName = simpleClassName

    val (protoMembers, functionInfos: List[Option[FunctionTypeInfo]]) =
    	classVars
    		.protoMembers
    		.sortBy(_.getName)
    		.map(convertMember(_, parentTypes))
    		.unzip

    val staticMembers =
    	classVars
    		.staticMembers
    		.sortBy(_.getName)
    		.filter(!_.getName.endsWith(".prototype"))
    		.map(convertMember(_, parentTypes)._1)

    // println("PROTO MEMBERS:\n\t" + protoMembers.mkString("\n\t"))
    // println("FUNCTION INFOS:\n\t" + functionInfos.mkString("\n\t"))

    val tparamDefs = {
    	val inferredTemplateParams: List[List[String]] =
    		functionInfos.flatten.flatMap(_.thisTemplateParams)

    	var ctparams = constructorInfo.map(_.templateParams)
    	if (simpleClassName == "Array")
    		ctparams = Some(List("T"))

    	val set: Set[List[String]] =
    		(inferredTemplateParams ++ ctparams)
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
          //@scalaxy.js.global
          object $companionName {
            ..$staticMembers
          }
        """ :: Nil

    lazy val traitTree =
      fixQuasiquotesInitSuperCall(q"""
        //@scalaxy.js.global
        trait $className {
          ..$protoMembers
        }
      """)

    val result = (constructorInfo, constructorInfo.flatMap(_.jsDocInfo)) match {
      case (None, None) =>
        traitTree :: companion
      case _ if classVars.constructor.get.getType.isInterface =>
        traitTree :: companion
      case (Some(info), Some(doc)) =>
        // TODO add template params from methods (e.g. for Array)
        val templateTypeNames = info.templateParamsSet

      	val (base: Tree, baseArgs: List[Tree]) =
      		baseTypeOpt.map(t => {
      			val superArgs = baseConstructorInfoOpt collect {
      				case baseInfo =>
      					info
      						.params
      						.take(baseInfo.params.size)
      						.map(_._1)
      						.map(n => Ident(n: TermName))
						} getOrElse(Nil)

      			convertTypeRef(t, templateTypeNames).tpt -> superArgs
    			}).getOrElse {
      			val base =
      				if (simpleClassName.matches("Object|Number|Boolean"))
	              TypeTree(typeOf[AnyRef])
	            else
	              resolver("Object": TypeName)
	          base -> Nil
          }

        val interfaces = interfaceTypes.map(convertTypeRef(_, templateTypeNames).tpt)

        // println(s"BASE = $base($baseArgs)")
        // println(s"\tinterfaces = $interfaces")

        val cparams = getParams(info, rename = true)
        val classDef = q"""
          //@scalaxy.js.global
          class $className[..$tparamDefs](..$cparams)
              extends $base(..$baseArgs) with ..$interfaces {
            ..$protoMembers
          }
        """
        fixQuasiquotesInitSuperCall(classDef) :: companion
      // }
    }
    val res = result.map(fixQuasiquotesInitSuperCall)

    // if (simpleClassName == "Transferable")
    // 	println("FOUND: " + res)
  	res
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
