

case class JunctionBox(x: Int, y: Int, z: Int) {

  override def equals(obj: Any): Boolean = {
    obj match {
      case another: JunctionBox => x == another.x && y == another.y && z == another.z
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val hash = x.hashCode() ^ y.hashCode() ^ z.hashCode()
    31 * hash
  }
}

case class Circuit(a: JunctionBox, b: JunctionBox, distance: Double) {

  override def equals(obj: Any): Boolean = {
    obj match {
      case another: Circuit => (a == another.a && b == another.b) || (a == another.b && b == another.a)
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val hash = a.hashCode() ^ b.hashCode()
    31 * hash + distance.hashCode()
  }
}

object CircuitOrdering extends Ordering[Circuit] {
  def compare(a: Circuit, b: Circuit): Int = a.distance.compare(b.distance)
}
