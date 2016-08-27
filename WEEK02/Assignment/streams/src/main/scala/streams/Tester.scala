package streams
import common._

object Notepad extends StringParserTerrain with App with Solver {
  val level =
    """------
        |--oo--
        |-ooSoo-
        |oooooooo-----
        |--ooTooo----
        |-oooooooo-
        |oooooo""".stripMargin
  println(level)
  val start = Block(Pos(2, 3), Pos(2, 3))
  //println(start.legalNeighbors) // only up and down
  //println(neighborsWithHistory(start, List(Left, Up)).take(2).toList)
  //println(terrainFunction(levels)(new Notepad.Pos(0,2)))
  //println(findChar('T', levels))
  //println(Block(Pos(2,4),Pos(2,4)).isStanding)
  //println(start.neighbors) // 

  //println(goal)
  //println(goal == Pos(4,3))

  println("nei with hist")
  println(neighborsWithHistory(start, List(Left, Up)).take(4).toList)

  println("new neighbours")
  println(newNeighborsOnly(neighborsWithHistory(start, List(Left, Up)), Set(start)).take(2).toList)
  //  println(newNeighborsOnly(neighborsWithHistory(start, List(Left, Up)), Set(Block(Pos(3,3),Pos(4,3)), start)).take(2).toList)

  println("paths from start")
  println(pathsFromStart.take(8).toList)

  println("paths to goal")
  println(pathsToGoal.take(3).toList)
}