package day04

import scala.io.Source
import com.github.nscala_time.time.Imports._

import scala.util.{Failure, Success, Try}


object Day04{
    val input = day04.Day04.getClass.getResource("input")
    val lines: List[String] = Source.fromURL(input).getLines().toList

    def main(args: Array[String]): Unit = {
        println(solve_first(lines))
        println(solve_second(lines))
    }

    private def parseLogDate(line: String): (DateTime, String) = {
        val dateTextSplitter = """\[(.+)\] (.+)""".r
        val matches = dateTextSplitter.findFirstMatchIn(line)
        val dateStr = matches.get.group(1)
        val date = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm").parseDateTime(dateStr)

        val patternGuard = """Guard #(\d+) begins shift""".r
        val text = matches.get.group(2) match {
            case patternGuard(id) => id
            case other => other
        }

        (date, text)
    }

    def groupDates(datesAndText: List[(DateTime, String)]): List[(Int, List[DateTime])] = {
        def groupGuardDates(guardDatesAndText: List[(DateTime, String)], id: Int, acc: List[DateTime]): List[(Int, List[DateTime])]  = {
            guardDatesAndText match{
                case (date, text)::ls =>
                    if (Try(text.toInt).isSuccess)
                        (id, acc)::groupGuardDates(ls, text.toInt, List(date))
                    else
                        groupGuardDates(ls, id, acc :+ date)
                case Nil => List((id, acc))
            }
        }
        val first::rest = datesAndText
        val (date, id) = (first._1, first._2.toInt)
        groupGuardDates(rest, id, List(date))
    }

    def solve_first(lines: List[String]): Int = {
        val dateAndText: List[(DateTime, String)] = lines.map(line => parseLogDate(line)).sortBy(a => a._1)
        val guardsHours = groupDates(dateAndText)
        0
    }

    def solve_second(lines: List[String]): Int = {
        0
    }
}
