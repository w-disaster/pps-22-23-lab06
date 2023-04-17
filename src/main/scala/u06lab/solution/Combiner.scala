package u06lab.solution

/** 1) Implement trait Functions with an object FunctionsImpl such that the code in
 *  TryFunctions works correctly. */

trait Functions:
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
  def combiner[A: Combiner](a: Seq[A]) : A

object FunctionsImpl extends Functions:
  override def sum(a: List[Double]): Double = a.foldLeft(0d)((c, d) => c + d)

  override def concat(a: Seq[String]): String = a.foldLeft("")((c, d) => c ++ d)

  override def max(a: List[Int]): Int = a.foldLeft(Int.MinValue)((b, c) => if b >= c then b else c)

  override def combiner[A: Combiner](a: Seq[A]) : A =
    a.foldLeft(summon[Combiner[A]].unit)((a, b) => summon[Combiner[A]].combine(a, b))

/*
 * 2) To apply DRY principle at the best,
 * note the three methods in Functions do something similar.
 * Use the following approach:
 * - find three implementations of Combiner that tell (for sum,concat and max) how
 *   to combine two elements, and what to return when the input list is empty
 * - implement in FunctionsImpl a single method combiner that, other than
 *   the collection of A, takes a Combiner as input
 * - implement the three methods by simply calling combiner
 *
 * When all works, note we completely avoided duplications..
 */

trait Combiner[A]:
  def unit: A
  def combine(a: A, b: A): A

object FunctionsCombiner:

  given Combiner[Double] with
    override def unit: Double = 0d
    override def combine(a: Double, b: Double): Double = a + b

  given Combiner[String] with
    override def unit: String = ""
    override def combine(a: String, b: String): String = a ++ b

  given Combiner[Int] with
    override def unit: Int = Int.MinValue
    override def combine(a: Int, b: Int): Int = if a >= b then a else b

import FunctionsCombiner.given

@main def checkFunctions(): Unit =
  val f: Functions = FunctionsImpl
  println(f.combiner(List(10.0, 20.0, 30.1))) // 60.1
  println(f.combiner(List())) // 0.0
  println(f.combiner(Seq("a", "b", "c"))) // abc
  println(f.combiner(Seq())) // ""
  println(f.combiner(List(-10, 3, -5, 0))) // 3
  println(f.combiner(List())) // -2147483648
