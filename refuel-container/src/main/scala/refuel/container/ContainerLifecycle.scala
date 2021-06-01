package refuel.container

trait ContainerLifecycle {
  def container: Container
  def ip: InjectionPool
}
