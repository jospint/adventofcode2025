import java.util
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParSet
import scala.io.Source

case class MachineForVoltage(buttonWirings: Array[ButtonWiring], joltageReqs: VoltageState)

type ButtonWiring = Array[Short]

final class VoltageState(val voltages: Array[Short]) {
  override val hashCode: Int = util.Arrays.hashCode(voltages)

  override def equals(obj: Any): Boolean = obj match {
    case other: VoltageState => util.Arrays.equals(voltages, other.voltages)
    case _ => false
  }

}

/* This blows up on large inputs, not an actual solution */
@main def Day10Part2(fileName: String): Unit = {
  def parseMachine(line: String): MachineForVoltage = {
    val machine = line.split(' ').toSeq

    val buttonWirings = machine.tail.reverse.tail.reverse.map(_.replace("(", "").replace(")", "")).map(_.split(',').map(_.toShort).toArray).toArray
    val joltageReqs = VoltageState(machine.reverse.head.replace("{", "").replace("}", "").split(',').map(_.toShort))
    MachineForVoltage(buttonWirings, joltageReqs)
  }

  def calculatePresses(machine: MachineForVoltage): Int = {

    val totalVoltage = machine.joltageReqs.voltages.sum
    val limit = 100000

    def updateVoltage(currentVoltage: VoltageState, buttonWiring: ButtonWiring): VoltageState = {
      val newVoltages = currentVoltage.voltages.clone()
      buttonWiring.foreach { index =>
        newVoltages(index) = (newVoltages(index) + 1).toShort
      }
      VoltageState(newVoltages)
    }

    @tailrec
    def calculatePressesRec(machine: MachineForVoltage, currentVoltagesForLevel: ParSet[VoltageState], iteration: Int): Int = {
      if (currentVoltagesForLevel.isEmpty) throw new Exception("Unlucky")
      println(s"${machine.joltageReqs.voltages.mkString(",")} with ${machine.buttonWirings.mkString("*")} iteration in progress $iteration, ${currentVoltagesForLevel.size}")
      val startTime = System.currentTimeMillis()
      val newCombos = currentVoltagesForLevel.par.flatMap { currentVoltage =>
        machine.buttonWirings.iterator.map(updateVoltage(currentVoltage, _))
          .filter(combo => combo.voltages.zip(machine.joltageReqs.voltages).map { (combo, voltage) => voltage - combo }.forall(delta => delta >= 0))
      }
      println(s"Solve step ${System.currentTimeMillis() - startTime}")
      if (newCombos.contains(machine.joltageReqs)) {
        iteration
      } else {
        println(s"Filter step ${System.currentTimeMillis() - startTime}")
        val size = newCombos.size
        if (newCombos.size > limit) {
          val combosWithDeltas = newCombos.map { combo =>
            val totalDelta = totalVoltage - combo.voltages.sum
            val individualDeltas = combo.voltages.zip(machine.joltageReqs.voltages).map { (combo, voltage) => voltage - combo }
            (individualDeltas, totalDelta, combo)
          }.toList
          println(s"Combos with deltas ${System.currentTimeMillis() - startTime}")
          val bestCombosByTotalVoltage = combosWithDeltas.sortBy((individualDelta, totalDelta, combo) => totalDelta).take(size / 10).map((delta, totalDelta, combo) => combo).toSet
          println(s"Sort by total deltas ${System.currentTimeMillis() - startTime}")
          val bestCombosByIndividualVoltages = machine.joltageReqs.voltages.indices.par.map(index =>
            combosWithDeltas.sortBy { (delta, totalDelta, combo) => delta(index) }.take(size / 10).map((delta, totalDelta, combo) => combo).toSet
          ).toSet.flatten
          println(s"Sort by individual deltas ${System.currentTimeMillis() - startTime}")
          val bestCombos = bestCombosByTotalVoltage ++ bestCombosByIndividualVoltages
          println(s"Selection step ${System.currentTimeMillis() - startTime}")
          calculatePressesRec(machine, bestCombos.par, iteration + 1)
        } else {
          calculatePressesRec(machine, newCombos, iteration + 1)
        }
      }
    }

    val initialVoltages = machine.buttonWirings.map(updateVoltage(VoltageState(Array.fill(machine.joltageReqs.voltages.length)(0.toShort)), _))

    if (initialVoltages.contains(machine.joltageReqs)) {
      1
    } else {
      calculatePressesRec(machine, initialVoltages.par.toSet, 2)
    }
  }

  val source = Source.fromFile(fileName)
  val machines = source.getLines().toSeq.map(parseMachine)
  val result = machines.map(calculatePresses).sum

  println(s"The fewest amount of presses is $result")
  source.close()
}
