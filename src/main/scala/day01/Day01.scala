package day01

import scala.io.Source

object Puzzle
{
  def main(args: Array[String]): Unit = {
    val resource = day01.Puzzle.getClass.getResource("frequencies.txt")
    val iter: Iterator[String] = Source.fromURL(resource).getLines()
    val list: List[Int] = iter.map(_.toInt).toList
    val sum: Int = list.sum
    println(f"Result = $sum")
    val firstDuplicate = firstFreqReachedTwice(list)
    print(f"Duplicate = $firstDuplicate")
  }

  def firstFreqReachedTwice(frequencies: List[Int]): Int ={
    firstDuplicate(frequencies, frequencies, Set.empty[Int], 0)
  }

  def firstDuplicate(numbers:List[Int], numbers_tail: List[Int], seenNumbers: Set[Int], curSum: Int): Int = {
    numbers_tail match {
      case Nil => firstDuplicate(numbers, numbers, seenNumbers, curSum)
      case x::tail => {
        val sum = curSum + x
        if (seenNumbers.contains(sum)){
          println(f"found $sum")
          sum
        }
        else {
          val newSeenNumbers = seenNumbers + sum
          firstDuplicate(numbers, tail, newSeenNumbers, sum)
        }
      }
    }
  }
}