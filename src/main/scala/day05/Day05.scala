package day05

import java.net.URL

import scala.io.Source


object Day05 {
    val input: URL = day05.Day05.getClass.getResource("input")
    val lines: List[String] = Source.fromURL(input).getLines().toList

    def main(args: Array[String]): Unit = {
        val line = lines.head
        println(solve_first(line))
        //println(solve_second(lines))
    }

    def solve_first(line: String): String = {
        def reduceString(input: Seq[Char], acc: Seq[Char]): String = {
          input match {
              case a+:b+:rest =>
                  if (a != b && a.toLower == b.toLower)
                      reduceString(acc ++ rest, Vector.empty[Char])
                  else
                      reduceString(b+:rest, acc :+ a)
              case b+:IndexedSeq() => (acc :+ b).mkString
              case IndexedSeq() => acc.toString
          }
        }
        reduceString(line.toVector, Vector.empty[Char])
    }

    //def solve_second(lines: String): Int = {
    //}
}
