package TimingMazes

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.PriorityQueue

/**
  * Created by ha8040we-s on 30/03/17.
  */
object MazeSolver{

  type Maze = Vector[Vector[Boolean]]

  def astar(maze: Maze): List[(Int, Int)] = {
      val graph = Graph(maze)
      val lookedAtNodes = mutable.HashSet[Node]()
      val weights = mutable.HashMap[Node, (Int, Int, Node)]() //weight, distance to end, node that leads to this node
      val queue = PriorityQueue[(Int, Node)]((0, graph.start))(Ordering.by{e: (Int, Node) => -e._1})


      while(!queue.isEmpty){
        val current = queue.dequeue()
        current._2.visited = true;
        if(current == graph.end)
            return reconstructPath(current) //only exit point
        else  
          for (val neighbor: Node <- current._2.neighbors if !neighbor.visited)) { //ignores nodes that we already found the shortest path to. aka already evaluated nodes
            calcWeight(current, neighbor)
          }
        
    } //add error if while loop is exited

      def calcWeight(from: Node, to: Node){
      val dist = from.distanceTo(to)
        weights.get(to) match {
          case Some(n) =>
                val weight = dist + n._2
                if( weight < n._1){
                weights += ((to,(weight ,n._2 ,from)))
                queue += ((weight, to))
                }
          case None =>
                distToEnd = graph.end.distanceTo(to)
                val weight = dist + distToEnd
                weights += ((to,(weight,distToEnd ,from)))
                queue += ((weight, to))
        }
      }

    def reconstructPath(current: Node): List((Int, Int)) = {
      current match {
        case graph.start => List()
        case _ => current.position :: reconstructPath(weights(current)._3)
      }
    }

  }

  def dijkstra(maze: Maze): List[(Int, Int)] = ???

  def deepfirst(maze: Maze): List[(Int, Int)] = ???

  def wallfollower(maze: Maze): List[(Int, Int)] = ???

}


case class Graph private (start: Node, end: Node, count: int, width: int, hight: int){

  case class Node(postion: (int, int), var visited = false){
    val neighbors: Array[Node] = Array[null, null, null, null] //left, up, down, right
    //the manhattan distance between two nodes
    def distanceTo(that: Node): int = math.abs(postion._1 - that.postion._1) + math.abs(postion._2 - that.postion._2)
  }
}


//algoritmen pajar om det finns en väg som är omringad av väggar
object Graph {

  type Maze = Vector[Vector[Boolean]]

  def apply(maze: Maze): Graph = {

    val width = maze(0).size
    val hight = maze.size
    var count = 2 // one entry and one exit node
    var entry: Node = null
    var exit: Node = null
    val bufferTop: Array[Node] = Array[width]

    //firstRow
    var foundNode = false
    var i = 1 //då första elementet alltid är false då det är en vägg

    while(!foundNode && i < width){
      if(maze(i)){
        entry = Node((0,i))
        bufferTop(i) = entry
        foundNode = true
      }
      i += 1
    }

    //all rows until the last one
    for(y <- 1 until hight -1){

      var leftNode: Node = null

      var prv = false
      var cur = false
      var nxt = maze(y)(1)

      for(x <- 1 until width)

      val node: Option[Node] = if(cur){
        if(prev){
          if(nxt){ //PathPathPath

            if(maze(y-1)(x) || maze(y+1)(x)){
              val n = Node((y,x))
              leftNode.neighbors(3) = n
              n.neighbors(0) = leftNode
              leftNode = n
              Some(n) //So that it is correctly evaluated

            } else None
          } else { // PathPathWall
          val n = Node((y,x))
            leftNode.neighbors(3) = n
            a.neighbors(0) = leftNode
            leftNode = None
            Some(n)  //So that it is correctly evaluated
          }

        } else {
          if(nxt){ //WallPathPath
          val n = Node((y,x))
            leftNode = n
            Some(n)  //So that it is correctly evaluated

          } else { // WallPathWall
            if(!(maze(y-1)(x) && maze(y+1)(x))){ // check if they are dead ends
              Some(Node((y,x)))
            }
          }

        }

        node match{
          // If node isn't none, we can assume we can connect N-S somewhere
          case Some(n) =>
            // Clear above, connect to waiting top node
            if (maze(y-1)(x)) {
              t = bufferTop(x)
              t.Neighbours(2) = n
              n.Neighbours(1) = t
            }
            // If clear below, put this new node in the top row for the next connection
            bufferTop(x) = if (data(y+1)(x)) n else null
          case _ =>

        }

        count += 1

      }


      //last row
      foundNode = false
      i = 1 //då första elementet alltid är false då det är en vägg

      while(!foundNode && i < width){
        if(maze(i)){
          exit = Node((hight-1,i))
          t = bufferTop(i)
          t.Neighbours(2) = exit
          exit.Neighbours(1) = t
          foundNode = true
        }
        i += 1
      }



    }

    new Graph(entry, exit, count, width, hight)
  }

}
