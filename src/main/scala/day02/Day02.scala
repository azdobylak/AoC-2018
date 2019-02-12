package day02

import scala.io.Source

object Day02
{
    def main(args: Array[String]): Unit = {
        val input = day02.Day02.getClass.getResource("input")
        val lines: List[String] = Source.fromURL(input).getLines().toList

        val solution = solve_second(lines)
        print(solution)
    }

    def solve_first(lines: List[String]): Int = {
        var doubles = 0
        var triples = 0
        for(line <- lines) {
            val charCount: Map[Char, Int] = line
                .groupBy(identity)
                .mapValues(_.size)
            doubles += (if (charCount.exists(_._2 == 2)) 1 else 0)
            triples += (if (charCount.exists(_._2 == 3)) 1 else 0)
        }
        doubles * triples
    }

    def solve_second(lines: List[String]): String = {
        val linePairs: List[(String, String)] = lines.combinations(2).toList.map({
            case List(x, y, _*) => (x, y)
        })

        val matchingChars: List[String] = linePairs.map{case (a, b) => findMatchingChars(a, b)}
        matchingChars.maxBy(_.length)
    }

    private def findMatchingChars(a: String, b: String): String = {
        (a zip b).filter(p => p._1 == p._2).map(_._1).mkString
    }

}
