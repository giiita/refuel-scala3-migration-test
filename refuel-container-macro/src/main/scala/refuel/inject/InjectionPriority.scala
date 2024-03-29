package refuel.inject

/**
  * Arbitrarily extensible priority object.
  * If extending, it must be a case object.
  */
trait InjectionPriority {
  val v: Int
}

/**
  *
  * Sample for defining any priority.
  *
  * ```
  * case object Secondary extends InjectionPriority(Primary >>)
  * case object SemiFinally extends InjectionPriority(Finally <<)
  * ```
  *
  * Define priority objects below Finally or Default assuming that they will be overwritten.
  *
  */
object InjectionPriority {

  /**
    * It will be the highest priority automatic injection.
    * No manual override of dependencies.
    */
  type Primary = _1
  val Primary = _1

  /**
    * It will be the highest priority automatic injection.
    * No manual override of dependencies.
    */
  type Secondary = _3
  val Secondary = _3

  trait _1 extends InjectionPriority {
    val v = 1
  }
  trait _2 extends _1 {
    override val v = 2
  }
  trait _3 extends _2 {
    override val v = 3
  }
  trait _4 extends _3 {
    override val v = 4
  }
  trait _5 extends _4 {
    override val v = 5
  }
  trait _6 extends _5 {
    override val v = 6
  }
  trait _7 extends _6 with _5 {
    override val v = 7
  }
  trait _8 extends _7 {
    override val v = 8
  }
  trait _9 extends _8 {
    override val v = 9
  }
  trait _10 extends _9 {
    override val v = 10
  }

  /**
    * Used in normal declarations.
    * Even if [[refuel.domination.Inject]] annotation is not added, it will be ranked as Default.
    * With custom injection priority you can override this.
    */
  type Overwrite = _5
  val Overwrite = _5

  /**
    * Used in normal declarations.
    * Even if [[refuel.domination.Inject]] annotation is not added, it will be ranked as Default.
    * With custom injection priority you can override this.
    */
  type Default = _7
  val Default = _7

  /**
    * It is injected as a substitute when there is no injection candidate.
    * If there are other similar definitions, they will definitely be overwritten.
    */
  type Finally = _10
  val Finally = _10

  implicit object _1  extends _1
  implicit object _2  extends _2
  implicit object _3  extends _3
  implicit object _4  extends _4
  implicit object _5  extends _5
  implicit object _6  extends _6
  implicit object _7  extends _7
  implicit object _8  extends _8
  implicit object _9  extends _9
  implicit object _10 extends _10
}
