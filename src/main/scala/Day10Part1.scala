import scala.annotation.tailrec
import scala.collection.BitSet
import scala.io.Source

case class Machine(lightDiagram: BitSet, buttonWirings: Seq[BitSet])

@main def Day10Part1(fileName: String): Unit = {
  def parseMachine(line: String): Machine = {
    val machine = line.split(' ').toSeq
    val lightDiagram = machine.head.replace("[", "").replace("]", "").zipWithIndex.collect { case ('#', idx) => idx }
    val lightDiagramBitSet = BitSet(lightDiagram *)

    val buttonWirings = machine.tail.reverse.tail.reverse.map(_.replace("(", "").replace(")", "")).map(_.split(',').map(_.toInt).toSeq).map(BitSet(_ *))
    Machine(lightDiagramBitSet, buttonWirings)
  }

  def calculatePresses(machine: Machine): Int = {
    @tailrec
    def calculatePressesRec(machine: Machine, buttonWiringsPressedForLevel: Seq[BitSet], iteration: Int): Int = {
      //println(s"${machine.lightDiagram} with ${machine.buttonWirings.mkString("*")} iteration in progress")
      val (isSolvedArray, newCombos) = buttonWiringsPressedForLevel.flatMap { buttonWiringsPressed =>
        machine.buttonWirings.map { buttonWiring =>
          val newCombo = buttonWiringsPressed ^ buttonWiring
          val isSolved = machine.lightDiagram == newCombo
          (isSolved, newCombo)
        }
      }.unzip
      if (isSolvedArray.contains(true)) {
        iteration
      } else {
        calculatePressesRec(machine, newCombos, iteration + 1)
      }
    }

    if (machine.buttonWirings.contains(machine.lightDiagram)) {
      1
    } else {
      calculatePressesRec(machine, machine.buttonWirings, 2)
    }
  }

  val source = Source.fromFile(fileName)
  val machines = source.getLines().toSeq.map(parseMachine)
  val result = machines.map(calculatePresses).sum

  println(s"The fewest amount of presses is $result")
  source.close()
}
