package day06

import java.net.URL

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex


object Day06 {
    val input: URL = day06.Day06.getClass.getResource("input")
    val lines: List[String] = Source.fromURL(input).getLines().toList

    def main(args: Array[String]): Unit = {
        println(solve_first(lines))
        println(solve_second(lines, 10000))
    }

    private def parse_cordinates(lines: List[String]): List[(Int, Int)] = {
        val cordsPattern: Regex = """(\d+), (\d+)""".r
        lines.map(s => {
                val matches = cordsPattern.findAllIn(s)
                (matches.group(1).toInt, matches.group(2).toInt)
            }
        )
    }

    private def distances(coordinate: (Int, Int), coordinates: List[(Int, Int)]): List[((Int, Int), Int)] = {
        coordinates.map(p => (p, Math.abs(p._1 - coordinate._1) + Math.abs(p._2 - coordinate._2)))
    }

    private def findClosest(coordinate: (Int, Int), coordinates: List[(Int, Int)]): (Int, Int) = {
        distances(coordinate, coordinates).minBy(_._2)._1
    }

    def solve_first(lines: List[String]): Int = {
        val coordinates = parse_cordinates(lines)
        val cordsX = coordinates.map(_._1)
        val cordsY = coordinates.map(_._2)
        var closestPoints: mutable.MutableList[(Int, Int)] = mutable.MutableList.empty[(Int, Int)]

        for(x <- cordsX.min + 1 to cordsX.max - 1)
            for(y <- cordsY.min + 1 to cordsY.max -1)
                closestPoints += findClosest((x, y), coordinates)

        val closestPointsCount: Map[(Int, Int), Int] = closestPoints.groupBy(identity).mapValues(_.length)
        closestPointsCount.maxBy(_._2)._2
    }

    def solve_second(lines: List[String], maxDistance: Int): Int = {
        val coordinates = parse_cordinates(lines)
        val cordsX = coordinates.map(_._1)
        val cordsY = coordinates.map(_._2)
        var isWithinRegion: mutable.MutableList[Boolean] = mutable.MutableList.empty[Boolean]

        for(x <- cordsX.min + 1 to cordsX.max - 1)
            for(y <- cordsY.min + 1 to cordsY.max -1)
            {
                val dists = distances((x, y), coordinates)
                isWithinRegion += dists.map(_._2).sum < maxDistance
            }

        isWithinRegion.count(identity)
    }
}
