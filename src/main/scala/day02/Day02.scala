package day02

import scala.io.Source

object Day02
{
    def main(args: Array[String]): Unit = {
        val input = day02.Day02.getClass.getResource("input")
        val lines: List[String] = Source.fromURL(input).getLines().toList

        val solution = solve(lines)
        print(solution)
    }

    def solve(lines: List[String]): Int = {
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
}
