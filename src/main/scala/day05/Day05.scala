package day05

import java.net.URL

import scala.io.Source


object Day05 {
    val input: URL = day05.Day05.getClass.getResource("input")
    val lines: List[String] = Source.fromURL(input).getLines().toList

    def main(args: Array[String]): Unit = {
        val line = lines.head
        //println(solve_first(line))
        println(solve_second(line))
    }

    def solve_first(line: String): Int = {
        reduce(line).length
    }

    def solve_second(line: String): Int = {
        var lengthWithoutChar: Map[Char, Int] = Map.empty[Char, Int]
        val chars = line.toLowerCase.toSet
        for (c <- chars)
            lengthWithoutChar += (c -> reduce(line, c).length)

        lengthWithoutChar.minBy(_._2)._2
    }

    private def removeCharacters(string: String, char: Char): String = {
        // remove all occurrences of given character, both upper and lowercase
        val excludeChars = Set(char.toLower, char.toUpper)
        string.filterNot(excludeChars)
    }

    def reduce(text: String, removeChar: Char): String = {
        val trimmedText = removeCharacters(text, removeChar)
        reduce(trimmedText)
    }

    def reduce(text: String): String = {
        def reduceString(input: Seq[Char], acc: Seq[Char]): String = {
            input match {
                case a +: b +: rest =>
                    if (a != b && a.toLower == b.toLower)
                        reduceString(acc ++ rest, Vector.empty[Char])
                    else
                        reduceString(b +: rest, acc :+ a)
                case b +: IndexedSeq() => (acc :+ b).mkString
                case IndexedSeq() => if (acc.isEmpty) "" else acc.toString
            }
        }
        reduceString(text.toVector, Vector.empty[Char])
    }

}
