package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait EmptyLevel extends SolutionChecker {
    val level = ""
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function empty level") {
    val l  = new EmptyLevel {
      assert(!terrain(Pos(0,0)), "empty 0,0")
      assert(!terrain(Pos(1,1)), "empty 1,1")
      assert(!terrain(Pos(4,7)), "empty 4,7")
      assert(!terrain(Pos(5,8)), "empty 5,8")
      assert(!terrain(Pos(5,9)), "empty 5,9")
      assert(!terrain(Pos(4,9)), "empty 4,9")
      assert(!terrain(Pos(6,8)), "empty 6,8")
      assert(!terrain(Pos(4,11)), "empty 4,11")
      assert(!terrain(Pos(-1,0)), "empty -1,0")
      assert(!terrain(Pos(0,-1)), "empty 0,-1")
    }
    print (l.level)
  }


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(goal == Pos(4,7))
    }
  }

  test("block.isStanding function, level1") {
    new Level1 {
      val p0 = Pos(0, 0)
      val p1 = Pos(0, 1)
      val b0 = Block(p0, p0)
      val b1 = Block(p0, p1)
      assert(b0.isStanding, "0,0 - 0,0")
      assert(!b1.isStanding, "0,0 - 0,1")
    }
  }

  test("block.isLegal function, level1") {
    new Level1 {
      val p0 = Pos(0, 0)
      val p1 = Pos(0, 1)
      val p2 = Pos(0, 2)
      val p3 = Pos(0, 3)
      val p4 = Pos(5, 8)
      val p5 = Pos(5, 9)
      val b0 = Block(p0, p0)
      val b1 = Block(p0, p1)
      val b2 = Block(p2, p3)
      val b3 = Block(p3, p3)
      val b4 = Block(p4, p5)
      assert(b0.isLegal, "0,0 - 0,0")
      assert(b1.isLegal, "0,0 - 0,1")
      assert(!b2.isLegal, "0,2 - 0,3")
      assert(!b3.isLegal, "0,3 - 0,3")
      assert(!b4.isLegal, "5,8 - 5,9")
    }
  }

  test("startBlock function, level1") {
    new Level1 {
      val p = Pos(1,1)
      val b = Block(p, p)
      assert(startBlock == b, "level 1 start block")
    }
  }

  test("neighbors function, level1") {
    new Level1 {
      val leftBlock = Block(Pos(1, -1), Pos(1, 0))
      val upBlock = Block(Pos(-1, 1), Pos(0, 1))
      val rightBlock = Block(Pos(1, 2), Pos(1, 3))
      val downBlock = Block(Pos(2, 1), Pos(3, 1))
      val neighbors = List((leftBlock, Left), (upBlock, Up), (rightBlock, Right), (downBlock, Down))
      assert(startBlock.neighbors == neighbors, "level 1, startBlock neighbors")
    }
  }

  test("legalNeighbors function, level1") {
    new Level1 {
      val rightBlock = Block(Pos(1, 2), Pos(1, 3))
      val downBlock = Block(Pos(2, 1), Pos(3, 1))
      val legalNeighbors = List((rightBlock, Right), (downBlock, Down))
      assert(startBlock.legalNeighbors == legalNeighbors, "level 1, startBlock legalNeighbors")
    }
  }

  test("done function, level1") {
    new Level1 {
      val p0 = Pos(0, 0)
      val p1 = Pos(0, 1)
      val p2 = Pos(4, 6)
      val p3 = Pos(3, 7)
      val b0 = Block(p0, p0)
      val b1 = Block(p0, p1)
      val b2 = Block(p2, goal)
      val b3 = Block(p3, goal)
      val b4 = Block(goal, goal)
      assert(!done(b0), "level 1, random standing block")
      assert(!done(b1), "level 1, random non-standing block")
      assert(!done(b2), "level 1, block close to goal")
      assert(!done(b3), "level 1, block close to goal 2")
      assert(done(b4), "level 1, standing block on the goal")
    }
  }

  test("neighborsWithHistory function, level1") {
    new Level1 {
      val rightBlock = Block(Pos(1, 2), Pos(1, 3))
      val downBlock = Block(Pos(2, 1), Pos(3, 1))
      val expected = List((rightBlock, List(Right)), (downBlock, List(Down)))
      assert(neighborsWithHistory(startBlock, List()) == expected, "level 1, startBlock neighborsWithHistory")

      
      val suggestedBlock = Block(Pos(1,1),Pos(1,1))
      val suggestedHistory = List(Left,Up)
      val suggestedResult = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream
      assert (neighborsWithHistory(suggestedBlock, suggestedHistory) == suggestedResult, "level 1, suggested example neighborsWithHistory")
    }
  }

  test("newNeighborsOnly function, level1") {
    new Level1 {
      val rightBlock = Block(Pos(1, 2), Pos(1, 3))
      val downBlock = Block(Pos(2, 1), Pos(3, 1))
      val neighbors = List((rightBlock, List(Right)), (downBlock, List(Down))) toStream
      val expected = List((rightBlock, List(Right)))
      assert(newNeighborsOnly(neighbors, Set(downBlock)) == expected, "level 1, startBlock newNeighborsOnly")


      val suggestedNeighbors = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream
      val suggestedExplored = Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      val suggestedResult = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream

      assert(newNeighborsOnly(suggestedNeighbors, suggestedExplored) == suggestedResult, "level 1, suggested example newNeighborsOnly")

    }
  }



  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
