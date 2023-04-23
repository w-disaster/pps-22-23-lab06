package u06lab.solution

import u06lab.solution.Solitaire.render

object Solitaire extends App:

  private val offsets: Seq[(Int, Int)] = Seq((-3, 0), (3, 0), (2, -2), (2, 2),
    (-2, 2), (-2, -2), (0, 3), (0, -3))

  private def computeNewPos(pos: (Int, Int)): Seq[(Int, Int)] = pos match
    case (x, y) => offsets.map((vx, vy) => (vx + x, vy + y))

  private def searchMarksPaths(initPos: (Int, Int), alreadyPos: Seq[(Int, Int)],
                               width: Int, height: Int): Seq[Seq[(Int, Int)]] =
    val pos = for
      v@(x, y) <- computeNewPos(initPos)
      if !alreadyPos.contains(v)
      if x >= 0 && x < width && y >= 0 && y < height
    yield searchMarksPaths(v, alreadyPos ++ Set(v), width, height)
    pos match
      case List() => Seq(alreadyPos)
      case _ => pos.flatten

  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
                    number = solution.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def placeAllMarks(width: Int, height: Int): Seq[Seq[(Int, Int)]] =
    for
      x <- 0 until width
      y <- 0 until height
      paths <- searchMarksPaths((x, y), Seq((x, y)), width, height).filter(s => s.size == width * height)
    yield paths

  def placeMarksFromCenter(width: Int, height: Int): Seq[Seq[(Int, Int)]] =
    val initPos = (width / 2, height / 2)
    searchMarksPaths(initPos, Seq(initPos), width, height).filter(s => s.size == width * height)

  private val width: Int = 5
  private val height: Int = 5

  /*
  private val allMarks = placeAllMarks(width, height)
  allMarks.foreach(path => println(render(path, width, height) + "\n"))
  println(s"Given a grid of size ($width, $height), there are ${allMarks.size} possible solutions " +
    s"of the Solitaire problem.")
  */

  private val marksFromCenter = placeMarksFromCenter(width, height)
  marksFromCenter.foreach(path => println(render(path, width, height) + '\n'))
  println(s"Given a grid of size ($width, $height) and the initial position (${width / 2}, ${height /2})," +
    s" there are ${marksFromCenter.size} possible solutions of the Solitaire problem")