object DFS_Traversal extends App{
  val graph = new Graph(6)
  graph.add_edge(1, 2)
  graph.add_edge(1, 3)
  graph.add_edge(1, 6)
  graph.add_edge(2, 3)
  graph.add_edge(3, 4)
  graph.add_edge(3, 6)
  graph.add_edge(4, 5)
  graph.add_edge(6, 4)
  graph.add_edge(5, 6)
  graph.dfs1(1)
  class Graph(numVertex: Int){
    var Adj_list = Map[Int,List[Int]]().withDefaultValue(Nil)
    def add_edge(u: Int, v: Int): Unit = {
      Adj_list = Adj_list + (u -> (v :: Adj_list(u)))
    }
    def dfs1(start: Int): Unit = {
      val Visited = Array.fill[Boolean](numVertex+1)(false)
      dfs(start, Visited)
    }
    def dfs(start: Int, Visited: Array[Boolean]): Unit = {
      Visited(start) = true
      println(start + " ")
      for(i <- Adj_list(start)){
        if(! Visited(i)){
          dfs(i,Visited)
        }
      }
    }
  }
}
