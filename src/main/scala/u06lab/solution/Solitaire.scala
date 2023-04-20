package u06lab.solution

import u06lab.solution.Solitaire.render

object Solitaire extends App:

  private val offsets: Seq[(Int, Int)] = Seq((-3, 0), (3, 0), (2, -2), (2, 2),
    (-2, 2), (-2, -2), (0, 3), (0, -3))
  private val width: Int = 5
  private val height: Int = 5

  def render(solution: Seq[(Int, Int)]): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
                    number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  private def computeNewPos(pos: (Int, Int)): Seq[(Int, Int)] = pos match
    case (x, y) => offsets.map((vx, vy) => (vx + x, vy + y))

  private def searchMarksPaths(initPos: (Int, Int), alreadyPos: Seq[(Int, Int)]):
  Seq[Seq[(Int, Int)]] =
    val pos = for
      v@(x, y) <- computeNewPos(initPos)
      if !alreadyPos.contains(v)
      if x >= 0 && x < width && y >= 0 && y < height
    yield searchMarksPaths(v, alreadyPos ++ Set(v))
    pos match
      case List() => Seq(alreadyPos)
      case _ => pos.flatten

  private def placeMarks(): Seq[Seq[(Int, Int)]] =
    for
      x <- 0 until width
      y <- 0 until height
      paths <- searchMarksPaths((x, y), Seq((x, y))).filter(s => s.size == width * height)
    yield paths

  private val allMarks = placeMarks()
  allMarks.foreach(path => println(render(path) + "\n"))
  println(s"There are ${allMarks.size} possible solutions of the Solitaire problem")