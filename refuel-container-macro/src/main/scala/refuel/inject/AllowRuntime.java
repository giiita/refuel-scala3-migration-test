package refuel.inject;


import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/**
 * Allowing dynamic dependency injection to be performed.
 * If you try to inject a class that does not have this annotation, you will always get a MacroInjection.
 * In Macro injection, it is possible to replace dependencies,
 * but if a candidate is not found at the time of compile, the compilation will fail.
 *
 * It is also possible to annotate each injection operation without annotating the injection base class.
 * {{{
 *   class Dependency extends AutoInject
 *
 *   // Be `new Dependency()`
 *   inject[Dependency@AllowRuntime]
 *
 *   // If a dependency is not found at compile time, the compile will fail.
 *   inject[Dependency]

 *   overwrite(Seq(1))
 *   // I won't find a predefinition that inherits the AutoInject of Seq[Int]
 *   inject[Seq[Int]]
 *   // However, if it is a RecognizedDynamicInjection, it will work.
 *   inject[Seq[Int]@AllowRuntime]
 * }}}
 *
 * Runtime injection can be very slow to load the first time due to the need to scan the classpath.
 * This can be avoided in many cases by using a constructor injection.
 * If you need support, please ask the commiter.
 */
@Retention(java.lang.annotation.RetentionPolicy.SOURCE)
@Target({ElementType.TYPE})
public @interface AllowRuntime {

}