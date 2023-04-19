package u06lab.code

import u06lab.code.Solitaire.render

object Solitaire extends App:
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  private val off: Seq[(Int, Int)] = Seq((-3, 0), (3, 0), (2, -2), (2, 2), (-2, 2), (-2, -2), (0, 3), (0, -3))
  private val size: Int = 5

  private def computeNewPos(pos: (Int, Int)): Seq[(Int, Int)] = pos match
    case (x, y) => off.map((vx, vy) => (vx + x, vy + y))

  private def nextPos(i: Int, initPos: (Int, Int), alreadyPos: Seq[(Int, Int)]): Unit = //Seq[(Int, (Int, Int))] =
    val pos = for
      v@(x, y) <- computeNewPos(initPos)
      if !alreadyPos.contains(v)
      if x >= 0 && x < size && y >= 0 && y < size
    yield (i, v)
    val res = for
      e @ (_, p) <- pos
    yield nextPos(i + 1, p, alreadyPos ++ Set(p))
    //Seq.concat(pos, res.flatten)
    println(alreadyPos ++ pos.map((_, v) => v))

  private def nextPos1(initPos: (Int, Int), alreadyPos: Seq[(Int, Int)]): Seq[Seq[(Int, Int)]] =
    val pos = for
      v@(x, y) <- computeNewPos(initPos)
      if !alreadyPos.contains(v)
      if x >= 0 && x < size && y >= 0 && y < size
    yield nextPos1(v, alreadyPos ++ Set(v))
    pos match
      case List() => Seq(alreadyPos)
      case _ => pos.flatten

  for
    x <- 0 until size
    y <- 0 until size
    ss <- nextPos1((x, y), Seq((x, y))).filter(s => s.size == 25)
  yield println(render(ss, size, size) + "\n")

