package day03

import java.util

import scala.io.Source
import scala.collection.mutable.Map

object Day03{
    val input = day03.Day03.getClass.getResource("input")
    val lines: List[String] = Source.fromURL(input).getLines().toList
    val fabric: Array[Array[Int]] = Array.ofDim[Int](lines.length, lines.length)
    val idsCount: Map[Int, Int] = Map.empty[Int, Int].withDefaultValue(0)

    def main(args: Array[String]): Unit = {
        println(solve_first(lines))
        println(solve_second(lines))
    }

    private def fillFabric(lines: List[String]): Unit = {
        for (i <- fabric.indices)
            fabric(i).transform(_ => 0)
        idsCount.clear
        val pattern = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r
        for (line <- lines){
            line match{
                case pattern(id, shift_width, shift_height, width, height) =>
                    val w = width.toInt
                    val sw = shift_width.toInt
                    val h = height.toInt
                    val sh = shift_height.toInt
                    val i = id.toInt
                    for(_width <- sw until sw + w; _height <- sh until sh + h){
                        fabric(_width)(_height) = if (fabric(_width)(_height) == 0) i else -1
                        idsCount(i) += 1
                    }
            }
        }
    }

    def solve_first(lines: List[String]): Int = {
        fillFabric(lines)
        fabric.flatten.count(_ < 0)
    }

    def solve_second(lines: List[String]): Int = {
        fillFabric(lines)
        val fabricCount = fabric.flatten.groupBy(identity).mapValues(_.length)
        (fabricCount.toSet intersect idsCount.toSet).head._1
    }
}