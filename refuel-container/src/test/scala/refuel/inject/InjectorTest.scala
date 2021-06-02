package refuel.inject

import org.scalatest.diagrams.Diagrams
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

object Xxx {
  object Module extends ModuleBase with AutoInject
  trait ModuleBase
}

class InjectorTest extends AsyncWordSpec with Matchers with Diagrams with Injector {
  "inject" should {
    "Standard inject with module" in {
      import Xxx.*
      val result = inject[ModuleBase]
      result shouldBe Module
    }
  }
}
