package TimingMazes

import java.io.File

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, PriorityQueue}

/**
  * Created by ha8040we-s on 30/03/17.
  */
object MazeSolver {

  type Maze = Vector[Vector[Boolean]]

  def astar(maze: Maze): List[(Int, Int)] = {
    val graph = Graph(maze)
    val lookedAtNodes = HashSet[Node]()
    val weights = HashMap[Node, (Int, Int, Node)]()
    //weight, distance to end, node that leads to this node
    val queue = PriorityQueue[(Int, Node)]((0, graph.start))(Ordering.by { e: (Int, Node) => -e._1 })
    var endLoop = false

    while (!queue.isEmpty && !endLoop) {
      val current = queue.dequeue()
      lookedAtNodes += current._2
      if (current._2 == graph.end)
        endLoop = true //only exit point
      else
        for (Some(neighbor) <- current._2.neighbors if !lookedAtNodes.contains(neighbor)) {
          //ignores nodes that we already found the shortest path to. aka already evaluated nodes
          calcWeight(current._2, neighbor)
        }

    } //last line

    def calcWeight(from: Node, to: Node) {
      val dist = from.distanceTo(to)
      weights.get(to) match {
        case Some(n) =>
          val weight = dist + n._2
          if (weight < n._1) {
            weights += ((to, (weight, n._2, from)))
            queue += ((weight, to))
          }
        case None =>
          val distToEnd = graph.end.distanceTo(to)
          val weight = dist + distToEnd
          weights += ((to, (weight, distToEnd, from)))
          queue += ((weight, to))
      }
    }

    def reconstructPath(current: Node): List[(Int, Int)] = {
      current match {
        case n@graph.start => n.position :: List()
        case _ => current.position :: reconstructPath(weights(current)._3)
      }
    }

    reconstructPath(graph.end)
  }

  def dijkstra(maze: Maze): List[(Int, Int)] = ???

  def depthFirst(maze: Maze): List[(Int, Int)] = {
    val graph = Graph(maze)
    val stack : mutable.Stack[Node] = mutable.Stack()
    search(graph.start)

    def search(current: Node): Unit = {
      //Lägg till nod i visited
      stack.push(current)
      //Om nod är mål:
      if (current != graph.end) {
        val neighbours = current.neighbors.filter(_.isDefined).map(_.get).filterNot(stack.contains(_))
        if (neighbours.nonEmpty) {
          for (n <- neighbours) {
            search(n)
          }
        } else {
          stack.pop()
        }
      }
    }

    stack.map(n => n.position).toList
  }

  def wallfollower(maze: Maze, start: (Int, Int), finish: (Int, Int)): List[(Int, Int)] = {
    val Up = (0, -1)
    val Down = (0, 1)
    val Left = (1, 0)
    val Right = (-1, 0)
    var direction = Left
    val coordBuffer = new scala.collection.mutable.ArrayBuffer[(Int, Int)]
    var (x, y) = start
    while ((x, y) != finish) {
      move()
      coordBuffer.append((x, y))
    }

    def readForward(maze: Vector[Vector[Boolean]]): Boolean = {
      maze(y + direction._2)(x + direction._1)
    }

    def walk() = {
      x += direction._1
      y += direction._2
    }

    def turnLeft(): (Int, Int) = {
      direction match {
        case Up => Left
        case Left => Down
        case Down => Right
        case Right => Up
      }
    }
    def turnRight() = {
      direction match {
        case Left => Up
        case Up => Right
        case Right => Down
        case Down => Left
      }
    }

    def move() = {
      turnLeft()
      if (!readForward(maze)) {
        turnRight()
        if (!readForward(maze)) {
          turnRight()
          if (!readForward(maze)) {
            turnRight()
          }
        }
      }
      walk()
    }
    coordBuffer.toList
  }
}

case class Graph private (start: Node, end: Node, count: Int, width: Int, hight: Int)

case class Node(position: (Int, Int)){
  val neighbors: Array[Option[Node]] = Array(None, None, None, None) //left, up, down, right
  //the manhattan distance between two nodes
  def distanceTo(that: Node): Int = math.abs(position._1 - that.position._1) + math.abs(position._2 - that.position._2)
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
    val bufferTop: Array[Node] = new Array(width)

    //firstRow
    var foundNode = false
    var i = 1 //då första elementet alltid är false då det är en vägg

    while(!foundNode && i < width){
      if(maze(0)(i)){
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

      for(x <- 1 until width - 1){

        prv = cur
        cur = nxt
        nxt = maze(y)(x+1)

        val node: Option[Node] = if(cur){
        if(prv){
          if(nxt){ //PathPathPath
            if(maze(y-1)(x) || maze(y+1)(x)){
              val n = Node((y,x))
              leftNode.neighbors(3) = Some(n)
              n.neighbors(0) = Some(leftNode)
              leftNode = n
              Some(n) //So that it is correctly evaluated
            } else None
          } else { // PathPathWall
          val n = Node((y,x))
            leftNode.neighbors(3) = Some(n)
            n.neighbors(0) = Some(leftNode)
            leftNode = null
            Some(n) //So that it is correctly evaluated
          }
        } else {
          if (nxt) {
            //WallPathPath
            val n = Node((y, x))
            leftNode = n
            Some(n) //So that it is correctly evaluated
          } else if (!(maze(y - 1)(x) && maze(y + 1)(x))) { //wallPathWall
                Some(Node((y, x)))
            } else None
        }
      } else None

        node match{
          // If node isn't none, we can assume we can connect N-S somewhere
          case Some(n) =>
            count += 1
            // Clear above, connect to waiting top node
            if (maze(y-1)(x)) {
              val t: Node = bufferTop(x)
              t.neighbors(2) = Some(n)
              n.neighbors(1) = Some(t)
            }
            // If clear below, put this new node in the top row for the next connection
            bufferTop(x) = if (maze(y+1)(x)) n else null
          case _ =>

        }
      }


      //last row
      foundNode = false
      i = 1 //då första elementet alltid är false då det är en vägg

      while(!foundNode && i < width){
        if(maze(hight-1)(i)){
          exit = Node((hight-1,i))
          val t = bufferTop(i)
          println(i)
          t.neighbors(2) = Some(exit)
          exit.neighbors(1) = Some(t)
          foundNode = true
        }
        i += 1
      }

    }

    new Graph(entry, exit, count, width, hight)
  }
}

//tests the graph constructor and astar
object Test {
  def main(args: Array[String]): Unit = {

    /*val maze =  Vector(
      Vector(false , true, false, false, false),
      Vector(false , true, true, true, false),
      Vector(false , true, false, true, false),
      Vector(false , false, false, true, false)
    )*/
    val maze = ImageReader.fromImage(new File("C:/Users/Tobbe/Desktop/maze10x10.png"))
    val correctValues = Vector((0,1),(1,1),(2,1),(1,3),(3,3))

    val alreadyChecked = new HashSet[Node]()
    //val graph = Graph(maze)
    //println(goThroughGraph(graph.start))
    println("number of nodes: " + alreadyChecked.size)

    MazeSolver.astar(maze).foreach(println(_))

    def goThroughGraph(node: Node): Boolean = {
      alreadyChecked.add(node)
      var result = true

      for(n <- node.neighbors if result){
        n match {
          case None => //annars matchar None på raden under
          case nd: Option[Node] if (!alreadyChecked.contains(nd.get)) =>
            if(correctValues.contains(nd.get.position))
              result = goThroughGraph(nd.get)
            else {
              result = false
              println(nd.get.position)
            }
          case _ =>
        }
      }
      result
    }
  }

}

object TestDFS {
  def main(args: Array[String]): Unit = {
    val maze =  Vector(
    Vector(false , true, false, false, false),
    Vector(false , true, true, true, false),
    Vector(false , true, false, true, false),
    Vector(false , false, false, true, false)
    )

    println(MazeSolver.depthFirst(maze))
  }
}