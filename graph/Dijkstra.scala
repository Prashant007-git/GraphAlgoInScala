object Dijkstra_Algo extends App{
  import scala.collection.mutable
  type Graph = Map[Int, Map[Int, Int]]
  def dijkstra(graph: Graph, startNode: Int): Map[Int, Int] = {
    val pq = mutable.PriorityQueue[(Int, Int)]()(Ordering.by(-_._2))
    val dist = mutable.Map[Int, Int]()
    for (node <- graph.keys) {
      dist(node) = if (node == startNode) 0 else Int.MaxValue
    }
    pq += startNode -> 0
    while (pq.nonEmpty) {
      val (currentNode, currentDist) = pq.dequeue()
      for ((neighbour, weight) <- graph(currentNode)){
        val newDist = currentDist + weight
        if (newDist < dist(neighbour)) {
          dist(neighbour) = newDist
          pq += neighbour -> newDist
        }
      }
    }
    dist.toMap
  }
  val graph: Graph = Map(
    1 -> Map(2 -> 1, 3 -> 4),
    2 -> Map(4 -> 3),
    3 -> Map(4 -> 1),
    4 -> Map()
  )
  val startNode = 1
  val shortestDistances = dijkstra(graph, startNode)
  println(shortestDistances)
}

