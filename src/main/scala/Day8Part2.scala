import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.io.Source

@main def Day8Part2(fileName: String): Unit = {

  def calculateDistance(a: JunctionBox, b: JunctionBox): Double = {
    Math.sqrt(Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2) + Math.pow(a.z - b.z, 2))
  }

  @tailrec
  def calculateDistancesInternal(box: JunctionBox, remainingBoxes: Array[JunctionBox], distances: HashSet[Circuit]): HashSet[Circuit] = {
    if (remainingBoxes.isEmpty) {
      distances
    } else {
      val boxToCompare = remainingBoxes.head
      val distance = calculateDistance(box, boxToCompare)
      val circuit = Circuit(box, boxToCompare, distance)
      if (distance == 0) {
        calculateDistancesInternal(box, remainingBoxes.tail, distances)
      } else {
        if (!distances.contains(circuit)) {
          calculateDistancesInternal(box, remainingBoxes.tail, distances + circuit)
        } else {
          calculateDistancesInternal(box, remainingBoxes.tail, distances)
        }
      }
    }
  }

  @tailrec
  def calculateShortestDistances(allBoxes: Array[JunctionBox], remainingBoxes: Array[JunctionBox], distances: HashSet[Circuit]): Array[Circuit] = {
    if (remainingBoxes.isEmpty) {
      distances.toArray.sorted(using CircuitOrdering)
    } else {
      val box = remainingBoxes.head
      val distancesWithBox = calculateDistancesInternal(box, allBoxes, distances)
      calculateShortestDistances(allBoxes, remainingBoxes.tail, distancesWithBox)
    }
  }

  @tailrec
  def makeGroups(numberBoxes: Int, remainingCircuits: Array[Circuit], groups: HashSet[HashSet[JunctionBox]], x1: Int, x2: Int): (Int, Int) = {
    if (remainingCircuits.isEmpty) {
      (x1, x2)
    } else {
      val circuit = remainingCircuits.head
      val containingGroup = groups.find(group => group.contains(circuit.a) || group.contains(circuit.b))
      val newGroups: HashSet[HashSet[JunctionBox]] = if (containingGroup.isEmpty) {
        groups ++ HashSet(HashSet(circuit.a, circuit.b))
      } else {
        val aGroup = groups.find(group => group.contains(circuit.a)).getOrElse(HashSet())
        val bGroup = groups.find(group => group.contains(circuit.b)).getOrElse(HashSet())
        val newGroup = aGroup ++ bGroup ++ HashSet(circuit.a, circuit.b)
        groups -- HashSet(aGroup) -- HashSet(bGroup) ++ HashSet(newGroup)
      }
      if (groups.size == 1 && newGroups.head.size == numberBoxes && x1 == -1 && x2 == -1) {
        (circuit.a.x, circuit.b.x)
      } else {
        makeGroups(numberBoxes, remainingCircuits.tail, newGroups, x1, x2)
      }
    }
  }

  val source = Source.fromFile(fileName)
  val lines = source.getLines()
  val boxes = lines.map(_.split(',')).map(xyz => JunctionBox(xyz(0).toInt, xyz(1).toInt, xyz(2).toInt)).toArray
  val shortestCircuits = calculateShortestDistances(boxes, boxes, HashSet())
  val groups = makeGroups(boxes.length, shortestCircuits, HashSet(), -1, -1)
  val result: Long = groups._1.toLong * groups._2.toLong
  println(s"The result is $result")

  source.close()

}

