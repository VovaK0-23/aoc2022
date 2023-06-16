import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Valve(flowRate: Int, tunnels: Array[String])
type Valves = Map[String, Valve]

@main def Main(args: String*): Unit = {
  args.headOption match {
    case Some(path) => {
      println(s"Input file: $path")
      val lines                  = Source.fromFile(path).getLines().toArray
      val (maxPressure, indices) = maxPressureCalculator(parseInput(lines))

      val p1 = maxPressure(30, "AA", 0)
      println(s"Part 1: $p1")

      var m = 0
      val b =
        (1 << indices.size) - 1 // n times 1 representing all closed valves
      for (i <- Range(0, b / 2)) {
        // println((b ^ i).toBinaryString)
        // Iterate over half of the possible combinations of opening valve orders
        // The division by 2 optimizes the loop by considering only one half of the combinations.
        // The other half is covered by the bitwise XOR operation (b ^ i) without requiring adjustment.
        m = m.max(maxPressure(26, "AA", i) + maxPressure(26, "AA", b ^ i))
      }

      println(s"Part 2: $m")
    }
    case None => println("Usage: scala Main <input-path>")
  }
}

def parseInput(
    lines: Array[String]
): Valves = {
  lines
    .foldLeft(
      mutable.Map[String, Valve]()
    ) { case (valvesAcc, line) =>
      val Array(valveInfo, tunnelsInfo) = line.split("; ")
      val Array(valveStr, flowRateStr)  = valveInfo.split(" has flow rate=")
      val flowRate                      = flowRateStr.toInt
      val valveName                     = valveStr.split("Valve ").last
      val tunnels =
        if (tunnelsInfo.contains(','))
          tunnelsInfo.split("valves ").last.split(", ").toArray
        else Array(tunnelsInfo.split("valve ").last)
      valvesAcc += (valveName -> Valve(flowRate, tunnels))
    }
    .toMap
}

def maxPressureCalculator(
    valves: Valves
): ((Int, String, Int) => Int, Map[String, Int]) = {
  val (dists, indices) = preprocessValves(valves)
  val cache            = mutable.Map[(Int, String, Int), Int]()

  // The dfs function calculates the maximum pressure that can be achieved
  // by moving from a given valve (name) at a given time (time)
  // with a specific combination of valves already released (determined by the bitmask).

  def dfs(time: Int, name: String, bitmask: Int): Int = {
    cache.getOrElseUpdate(
      (time, name, bitmask), {
        dists(name).foldLeft(0) { case (maxSoFar, (neighborName, dist)) =>
          val bit = 1 << indices(neighborName)
          if ((bitmask & bit) == 0) {
            val remtime = time - dist - 1
            if (remtime > 0) {
              val pressure = valves(neighborName).flowRate * remtime
              maxSoFar.max(dfs(remtime, neighborName, bitmask | bit) + pressure)
            } else maxSoFar
          } else maxSoFar
        }
      }
    )
  }

  return (dfs, indices)
}

def preprocessValves(
    valves: Valves
): (Map[String, Map[String, Int]], Map[String, Int]) = {
  val distances       = mutable.Map[String, Map[String, Int]]()
  val indicesNonempty = mutable.Map[String, Int]()

  valves.foreach { case (name, Valve(flowRate, tunnels)) =>
    if (name == "AA" || flowRate > 0) {
      val dists   = mutable.Map(name -> 0, "AA" -> 0)
      val visited = mutable.Set(name)
      val queue   = mutable.Queue((0, name))

      // BFS-like exploration of valves
      // This exploration is performed to find the shortest path
      // (number of edges) from current valve to each other valve.

      while (queue.nonEmpty) {
        val (dist, currName) = queue.dequeue()
        valves(currName).tunnels.foreach { neighborName =>
          if (!visited.contains(neighborName)) {
            visited.add(neighborName)
            if (valves(neighborName).flowRate > 0)
              dists += (neighborName -> (dist + 1))
            queue.enqueue((dist + 1, neighborName))
          }
        }
      }

      dists -= name
      if (name != "AA") {
        dists -= "AA"
        indicesNonempty += (name -> (indicesNonempty.size + 1))
      }
      distances += (name -> dists.toMap)
    }
  }

  (distances.toMap, indicesNonempty.toMap)
}
