package streams
import common._


object Notepad extends StringParserTerrain with App with Solver{
    val level =
      """------
        |--oo--
        |--oS--
        |--ooo-
        |---To-""".stripMargin
		val levels : Vector[Vector[Char]] = Vector(
			Vector('o', 'o', '-'),
			Vector('o', 'S', '-'),
			Vector('-', 'o', 'o', 'o', '-'),
			Vector('-', '-', 'T', 'o', '-')
		)
		println(level)
		val start = Block(Pos(2,2), Pos(2,3))
		println(start.legalNeighbors) // only up and down
		println(neighborsWithHistory(start, List(Left, Up)).take(2).toList)
		//println(terrainFunction(levels)(new Notepad.Pos(0,2)))
		//println(findChar('T', levels))
		//println(Block(Pos(2,4),Pos(2,4)).isStanding)
		//println(start.neighbors) // 
		
		//println(goal)
		//println(goal == Pos(4,3))

}