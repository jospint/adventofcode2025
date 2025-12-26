import scala.collection.mutable
import scala.io.Source

/**
 * Not my implementation. Scala port of solution from here
 * https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/
 * */
@main def Day10Part2Alt(fileName: String): Unit = {
  val source = Source.fromFile(fileName)
  solve(source.getLines().toVector)
  source.close()
}

def solve(lines: Vector[String]): Unit = {
  val score = lines.zipWithIndex.map { case (line, idx) =>
    val parts = line.split(" ").toVector
    val goalStr = parts.last
    val coeffStrs = parts.drop(1).dropRight(1)

    val goal = goalStr.drop(1).dropRight(1).split(",").map(_.toInt).toVector
    val numVars = goal.length

    val coeffs = coeffStrs.map { r =>
      val indices = r.drop(1).dropRight(1).split(",").map(_.toInt).toSet
      (0 until numVars).map(i => if (indices.contains(i)) 1 else 0).toVector
    }

    val subscore = solveSingle(coeffs, goal)
    println(s"Line ${idx + 1}/${lines.length}: answer $subscore")
    subscore
  }.sum

  println(score)
}

def solveSingle(coeffs: Vector[Vector[Int]], goal: Vector[Int]): Int = {
  val patternCosts = patterns(coeffs)
  val cache = mutable.Map.empty[Vector[Int], Int]

  def solveSingleAux(goal: Vector[Int]): Int = {
    if (goal.forall(_ == 0)) {
      0
    } else {
      cache.getOrElseUpdate(goal, {
        val parityPattern = goal.map(i => ((i % 2) + 2) % 2)
        patternCosts.getOrElse(parityPattern, Map.empty).foldLeft(1000000) { (answer, entry) =>
          val (pattern, patternCost) = entry
          if (pattern.zip(goal).forall { case (p, g) => p <= g }) {
            val newGoal = pattern.zip(goal).map { case (p, g) => (g - p) / 2 }
            answer.min(patternCost + 2 * solveSingleAux(newGoal))
          } else {
            answer
          }
        }
      })
    }
  }

  solveSingleAux(goal)
}

def patterns(coeffs: Vector[Vector[Int]]): Map[Vector[Int], Map[Vector[Int], Int]] = {
  val numButtons = coeffs.length
  val numVariables = coeffs.head.length

  val parityPatterns = (0 until (1 << numVariables)).map { i =>
    (0 until numVariables).map(j => (i >> j) & 1).toVector
  }

  val allPatterns = (0 to numButtons).flatMap { numPressedButtons =>
    (0 until numButtons).combinations(numPressedButtons).map { buttons =>
      val pattern = (0 until numVariables).map(v => buttons.map(b => coeffs(b)(v)).sum).toVector
      val parityPattern = pattern.map(_ % 2)
      (parityPattern, pattern, numPressedButtons)
    }
  }

  parityPatterns.map { parity =>
    val relevantPatterns = allPatterns
      .filter(_._1 == parity)
      .foldLeft(Map.empty[Vector[Int], Int]) { (acc, entry) =>
        val (_, pattern, cost) = entry
        if (acc.contains(pattern)) acc
        else acc + (pattern -> cost)
      }
    parity -> relevantPatterns
  }.toMap
}
