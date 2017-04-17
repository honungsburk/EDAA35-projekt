package TimingMazes

import java.io.{BufferedWriter, File, FileWriter}

  /**
    * Created by ha8040we-s on 30/03/17.
    */
  object MazeTimer {
    
    type Maze = Vector[Vector[Boolean]]
    
    def testAlgorithm(f: Maze => Maze, maze: Maze, utfile: File, iterations: Int) {
      val bw = new BufferedWriter(new FileWriter(utfile))
      for (i <- 1 until iterations) {
        val (time, result) = timer(f, maze)
        bw.write(i + " " + time + "\n")
      }
      bw.close()
    }

    def timer(f: Maze => Maze, maze: Maze): (Long, Maze) = {
      val startTime: Long = System.nanoTime

      val result = f(maze)

      val endTime: Long = System.nanoTime

      (startTime - endTime, result)
    }
  }

object MazeUtil {
  
  type Maze = Vector[Vector[Boolean]]
  
  import java.util.Scanner
  import java.io.File
  import scala.collection.mutable.ArrayBuffer
  
  def readMazeFromFile(file : File) : Maze = {
    val scanner = new Scanner(file)
    val tmpArray  = new ArrayBuffer[Vector[Boolean]]()
    val buffer = new ArrayBuffer[Boolean]()

    while(scanner.hasNextLine())
    {
      while(scanner.hasNextInt())
      {
        if(scanner.nextInt() == 1)
          buffer.append(true)
        else
          buffer.append(false)
      }
      tmpArray.append(buffer.toVector);
      if (scanner.hasNextLine)
      {
        scanner.nextLine();
      }
    }
    tmpArray.toVector
  }
}


  object Main {
    import MazeTimer._

    type Maze = Vector[Vector[Boolean]]

    //arg: infil, utfil, algoritm, ggr
    def main(args: Array[String]) {
      val infil = new File(args(0))
      val utfil = new File(args(1))
      val algoritm = args(2)
      val ggr = args(3).toInt


      val alg = algoritm match {
        case "astar" => MazeSolver.astar _
        case "dijkstra" => MazeSolver.dijkstra _
        case "deepfirst" => MazeSolver.depthFirst _
        case "wallfollower" => MazeSolver.wallfollower _
        case _ => throw new IllegalArgumentException("Did not found a algorithm with the corresponding name: " + algoritm)
      }

      val maze = MazeUtil.readMazeFromFile(infil)
      
      //testAlgorithm(alg, maze, utfil, ggr)

    }

  }
