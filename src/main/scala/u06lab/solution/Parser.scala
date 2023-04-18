package u06lab.solution

import u06lab.code.NotTwoConsecutiveParser
import u06lab.solution

/** Consider the Parser example shown in previous lesson. Analogously to NonEmpty, create a mixin NotTwoConsecutive,
  * which adds the idea that one cannot parse two consecutive elements which are equal. Use it (as a mixin) to build
  * class NotTwoConsecutiveParser, used in the testing code at the end. Note we also test that the two mixins can work
  * together!!
  */

abstract class Parser[T]:
  def parse(t: T): Boolean // is the token accepted?

  def end: Boolean // is it ok to end here

  def parseAll(seq: Seq[T]): Boolean = (seq forall parse) & end // note &, not &&

class BasicParser(chars: Set[Char]) extends Parser[Char]:
  override def parse(t: Char): Boolean = chars.contains(t)

  override def end: Boolean = true

trait NonEmpty[T] extends Parser[T]:
  private[this] var empty = true

  abstract override def parse(t: T): Boolean =
    empty = false;
    super.parse(t) // who is super??

  abstract override def end: Boolean = !empty && super.end

class NonEmptyParser(chars: Set[Char]) extends BasicParser(chars) with NonEmpty[Char]

trait NotTwoConsecutive[T] extends Parser[T]:

  private var prev: Option[T] = Option.empty

  abstract override def parse(t: T): Boolean =
    if !prev.equals(Option.apply(t)) then
      prev = Option.apply(t)
      super.parse(t)
    else false

class NotTwoConsecutiveParser(chars: Set[Char]) extends BasicParser(chars) with NotTwoConsecutive[Char]

/* Linearization:
    class A
    trait B extends (class) A
    trait C extends (class) A
    ...
    class D extends (class) A with (trait) B with (trait) C
*/
class ParserNTCNE(chars: Set[Char]) extends BasicParser(chars) with NotTwoConsecutive[Char] with NonEmpty[Char]

class ShorterThanNParser(n: Int) extends Parser[Seq[Char]]:

  override def parse(t: Seq[Char]): Boolean = t.size <= n

  override def end: Boolean = true

object Parsers:
  extension (s: String)
    def charParser(): BasicParser = BasicParser(s.toSet)


@main def checkParsers(): Unit =

  def parser = new BasicParser(Set('a', 'b', 'c'))
  println(parser.parseAll("aabc".toList)) // true
  println(parser.parseAll("aabcdc".toList)) // false
  println(parser.parseAll("".toList)) // true

  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = new NonEmptyParser(Set('0', '1'))
  println(parserNE.parseAll("0101".toList)) // true
  println(parserNE.parseAll("0123".toList)) // false
  println(parserNE.parseAll(List())) // false

  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  println(parserNTC.parseAll("XYZ".toList)) // true
  println(parserNTC.parseAll("XYYZ".toList)) // false
  println(parserNTC.parseAll("".toList)) // true

  // note we do not need a class name here, we use the structural type
  def parserNTCNE = new ParserNTCNE(Set('X', 'Y', 'Z'))
  println(parserNTCNE.parseAll("XYZ".toList)) // true
  println(parserNTCNE.parseAll("XYYZ".toList)) // false
  println(parserNTCNE.parseAll("".toList)) // false

  def parserShorterN = new ShorterThanNParser(3)
  println(parserShorterN.parse(Seq('X', 'Y', 'Z'))) // true
  println(parserShorterN.parse(Seq('X', 'Y', 'Z', 'Z', 'Q'))) // false

  import Parsers.*

  def sparser: Parser[Char] = "abc".charParser() // "abc".charParser()
  println(sparser.parseAll("aabc".toList)) // true
  println(sparser.parseAll("aabcdc".toList)) // false
  println(sparser.parseAll("".toList)) // true

