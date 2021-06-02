 package refuel.container.`macro`.internal

 import refuel.container.Config.AdditionalPackage
 import refuel.container.`macro`.internal.StaticDependencyExtractor.{isClass, isModule}
 import refuel.container.`macro`.internal.tools.LowLevelAPIConversionAlias
 import refuel.container.{Config, Container}
 import refuel.inject.{AutoInjectBase, InjectableSymbolHandler, InjectableTag}

 import scala.annotation.tailrec
 import scala.quoted._

 object StaticDependencyExtractor extends LowLevelAPIConversionAlias {
   private[this] var buffer: Option[Any] = None

   /**
    * Returns a list of dependencies found at compile time.
    * If a @RecognizedDynamicInjection is found in the search property,
    * I would expect runtime to be ranked together.
    * Otherwise, terminate the compilation or respond to the found candidates.
    *
    * @return [[ExcludingRuntime]] or [[ConfirmedCands]]
    */
   def searchInjectionCandidates[T: Type](using q: Quotes): Iterable[q.reflect.TypeTree] = {
     import q.reflect._

     val x                     = getList
     InjectableSymbolHandler.filterTargetSymbols[T](x)
   }

   private[this] def getList(using q: Quotes): Iterable[q.reflect.TypeTree] = {
     buffer match {
       case None =>
         run match {
           case x =>
             buffer = Some(x)
             x
         }
       case Some(x) => x.asInstanceOf[Iterable[q.reflect.TypeTree]]
     }
   }

   private[this] def unloadPackages(using q: Quotes): Seq[String] = {
     val config = Config.blackList
     config.collect {
       case AdditionalPackage(p) => p
     } match {
       case x if x.nonEmpty =>
         q.reflect.report.info(s"\nUnscanning injection packages:\n    ${x.mkString("\n    ")}\n\n")
       case _ =>
     }
     config.map(_.value)
   }

   private[this] def run(using q: Quotes): Iterable[q.reflect.TypeTree] = {
     recursivePackageExplore(
       unloadPackages,
       Vector(nealyPackage(q.reflect.TypeTree.of[Container].symbol))
     )
   }

   @tailrec
   private[this] final def nealyPackage(using q: Quotes)(current: q.reflect.Symbol): q.reflect.Symbol = {
     current.owner match {
       case x if x.isPackageDef && x.name == "<root>" => x
       case x                                            => nealyPackage(x)
     }
   }

   private[this] final def joinSymbolSet(using q: Quotes)(a: Iterable[q.reflect.TypeTree], b: Iterable[q.reflect.TypeTree]): Iterable[q.reflect.TypeTree] = {
     a ++ b
   }

   @tailrec
   private[this] final def recursivePackageExplore(using q: Quotes)(
                                              unloads: Seq[String],
                                              selfPackages: Vector[q.reflect.Symbol],
                                              injectableSymbols: Iterable[q.reflect.TypeTree] = None
                                            ): Iterable[q.reflect.TypeTree] = {
     selfPackages match {
       case x if x.isEmpty => injectableSymbols
       case _ =>
         val (packages, modules) = selfPackages.flatMap(_.declarations).distinct.collect {
           // duplicated || blacklist cases
           case x if {
             if(x.fullName == "org.scalatest.tools.ScalaTestAntTask") {
               println(x.fullName)
               println(try {x.tree} catch { case e => "er"})
             }
             selfPackages.contains(x) || unloads.contains(x.fullName)
           } =>
             None -> None
           // other packages
           case x if x.isPackageDef =>
             Some(x) -> None
           // if static object || class def and mixed in AutoInject then recursive find dependencies
           case x
             // [info]    |                                       class : object : module class
             // [info]    |                    isRefinementClass  false : false : false
             // [info]    |                    isAbstractType     false : false : false
             // [info]    |                    isClassConstructor false : false : false
             // [info]    |                    isType             true : false : true
             // [info]    |                    isTerm             false : true : false
             // [info]    |                    isClassDef         true : false : true
             // [info]    |                    isTypeDef          false : false : false
             // [info]    |                    isValDef           false : true : false
             // [info]    |                    isDefDef           false : false : false
             // [info]    |                    isBind             false : false : false
             if maybeCandidates(x) =>
               None -> Some(x)
         } match {
           case x => x.flatMap(_._1) -> x.flatMap(_._2)
         }

         recursivePackageExplore(
           unloads,
           packages,
           joinSymbolSet(injectableSymbols, recursiveModuleExplore(modules))
         )

     }
   }

   private[this] def maybeCandidates(using q: Quotes)(x: q.reflect.Symbol): Boolean = {
     var res: Boolean = false
     try {
       res = x != x.moduleClass
         && !x.isAbstractType
         && !x.flags.is(q.reflect.Flags.Abstract)
         && !x.flags.is(q.reflect.Flags.Trait)
         && !x.isNoSymbol
         && x.flags.show != ""
         && (isClass(x) || isModule(x))
     } catch { case e: Throwable =>
     }
     res
   }
   private[this] def isClass(using q: Quotes)(symbol: q.reflect.Symbol): Boolean = symbol.isType && symbol.isClassDef
   private[this] def isModule(using q: Quotes)(symbol: q.reflect.Symbol): Boolean = symbol.isTerm && symbol.isValDef

   @tailrec
   private[this]  final def recursiveModuleExplore(using q: Quotes)(
                                             n: Vector[q.reflect.Symbol],
                                             injectableSymbols: Iterable[q.reflect.TypeTree] = None
                                           ): Iterable[q.reflect.TypeTree] = {
     import q.reflect._
     n match {
       case accessibleSymbol if accessibleSymbol.isEmpty => injectableSymbols
       case accessibleSymbol =>
         val selection: Iterable[q.reflect.TypeTree] = accessibleSymbol.flatMap { x =>
           x.tree match {
             // (String, TypeTree, Option[Term])
             case ValDef(_, ttree, _) if ttree.tpe.<:<(InjectableTag()) =>
               Some(ttree)
             // (String, DefDef, List[Tree /* Term | TypeTree */], Option[ValDef], List[Statement])
             case ClassDef(_, _, _, _, _) if TypeIdent(x).tpe.<:<(InjectableTag()) =>
               Some(TypeIdent(x))
             case _ =>
               None
           }
         }
         recursiveModuleExplore(
           accessibleSymbol.withFilter(_.flags.is(Flags.Module)).flatMap(_.declarations).collect {
             case r if maybeCandidates(r) => r
           },
           joinSymbolSet(injectableSymbols, selection)
         )
     }
   }

//   @tailrec
//   private[this] final def recursiveStubTesting(x: c.Type*): Boolean = {
//     val prts = x.collect {
//       case api: ClassInfoTypeApi => api.parents
//       case typeRef: TypeRef =>
//         typeRef.typeSymbol.typeSignature match {
//           case _api: ClassInfoTypeApi => _api.parents
//           case PolyType(_, b)         => Seq(b)
//         }
//     }.flatten
//     if (prts.isEmpty) {
//       true
//     } else if (!prts.exists { pr =>
//       pr.typeSymbol.isClass && pr.typeSymbol.asClass
//         .isInstanceOf[scala.reflect.internal.Symbols#StubClassSymbol]
//     }) {
//       recursiveStubTesting(prts: _*)
//     } else {
//       false
//     }
//
//   }

//   def isInjectableOnce(using q: Quotes)(x: q.reflect.Symbol): Boolean =
//     try {
////         recursiveStubTesting(x.typeSignature) &&
//           x.isClass &&
//           !x.isAbstract &&
//           // brokenCheck(x) &&
//           x.asClass.primaryConstructor.isMethod &&
//           x.typeSignature.baseClasses.contains(AutoInjectionTag.typeSymbol)
//     } catch { e =>
//       false
//     }

   // private[this] def brokenCheck(value: c.Symbol): Boolean = {
   //   try {
   //     if (value.isModule) {
   //       c.typecheck(
   //         c.parse(
   //           value.fullName.replaceAll("(.+)\\$(.+)", "$1#$2")
   //         ),
   //         silent = true
   //       )
   //         .nonEmpty
   //     } else {
   //       c.typecheck(
   //         tree = c.parse(
   //           s"type `${c.freshName()}` = ${value.fullName.replaceAll("(.+)\\$(.+)", "$1.$2")}"
   //         ),
   //         silent = true
   //       )
   //         .nonEmpty
   //     }
   //   } catch {
   //     case _: Throwable => false
   //   }
   // }

//   implicit class RichVectorSymbol(value: Vector[Symbol]) {
//     def accessible: Vector[Symbol] = {
//       value
//       // value.filter(brokenCheck)
//     }
//   }

 }

