import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

@main def Main(args: String*): Unit = {
  val filePath = args.headOption
  filePath match {
    case Some(path) => {
      println(s"Input file: $path")
      val lines                = readFile(path)
      val (flowRates, tunnels) = parseInput(lines)
      val (dists, indices)     = calculateDistances(flowRates, tunnels)
      part1(dists, indices, flowRates)

    }
    case None => println("Usage: scala Main <input-path>")
  }
}

def readFile(fileName: String): Array[String] = {
  val source = Source.fromFile(fileName)
  try {
    val lines = source.getLines().toArray
    lines
  } finally {
    source.close()
  }
}

def parseInput(
    lines: Array[String]
): (Map[String, Int], Map[String, Array[String]]) = {
  val (flowRates, tunnels) = lines.foldLeft(
    (mutable.Map[String, Int](), mutable.Map[String, Array[String]]())
  ) { case ((flowRatesAcc, tunnelsAcc), line) =>
    val Array(valveInfo, tunnelsInfo) = line.split("; ")
    val Array(valveStr, flowRateStr)  = valveInfo.split(" has flow rate=")
    val flowRate                      = flowRateStr.toInt
    val valveName                     = valveStr.substring(6)
    val connectedValves = if (tunnelsInfo.contains(',')) {
      val Array(_, tunnelsStr) = tunnelsInfo.split("valves ")
      tunnelsStr.split(", ").toArray
    } else {
      Array(tunnelsInfo.substring(tunnelsInfo.length() - 2))
    }
    flowRatesAcc += (valveName -> flowRate)
    tunnelsAcc += (valveName   -> connectedValves)
    (flowRatesAcc, tunnelsAcc)
  }

  (flowRates.toMap, tunnels.toMap)
}

def calculateDistances(
    flowRates: Map[String, Int],
    tunnels: Map[String, Array[String]]
): (Map[String, Map[String, Int]], Map[String, Int]) = {
  val (allDists, indices) = flowRates.foldLeft(
    (mutable.Map[String, Map[String, Int]](), mutable.Map[String, Int]())
  ) { case ((distsAcc, indicesAcc), (name, flowRate)) =>
    if (name != "AA" && flowRate <= 0)
      (distsAcc, indicesAcc)
    else {
      val dists   = mutable.Map[String, Int](name -> 0, "AA" -> 0)
      val visited = mutable.Set[String](name)
      val queue   = mutable.Queue[(Int, String)]((0, name))

      while (queue.nonEmpty) {
        val (dist, currName) = queue.dequeue()
        for (neighborName <- tunnels(currName)) {
          if (!visited.contains(neighborName)) {
            visited.add(neighborName)
            if (flowRates(neighborName) > 0)
              dists += (neighborName -> (dist + 1))
            queue.enqueue((dist + 1, neighborName))
          }
        }
      }

      dists -= name
      if (name != "AA") {
        dists -= "AA"
        indicesAcc += (name -> (indicesAcc.size + 1))
      }

      distsAcc += (name -> dists.toMap)
      (distsAcc, indicesAcc)
    }
  }
  (allDists.toMap, indices.toMap)
}

def part1(
    dists: Map[String, Map[String, Int]],
    indices: Map[String, Int],
    flowRates: Map[String, Int]
) = {
  val cache = mutable.Map[(Int, String, Int), Int]()

  def dfs(time: Int, name: String, bitmask: Int): Int = {
    cache.getOrElseUpdate(
      (time, name, bitmask), {
        dists(name).foldLeft(0) { case (maxSoFar, (neighborName, dist)) =>
          val bit = 1 << indices(neighborName)
          if ((bitmask & bit) == 0) {
            val remtime = time - dist - 1
            if (remtime > 0) {
              val pressure = flowRates(neighborName) * remtime
              maxSoFar.max(dfs(remtime, neighborName, bitmask | bit) + pressure)
            } else maxSoFar
          } else maxSoFar
        }
      }
    )
  }

  print("Part 1: ")
  println(dfs(30, "AA", 0))
}
