package day04

import scala.io.Source
import com.github.nscala_time.time.Imports._

import scala.collection.mutable
import scala.collection.mutable.Buffer
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

    private def countAsleepMinutesPerGuard(guardsHours: List[(Int, List[DateTime])]): List[(Int, List[Int])] = {
        guardsHours.map {
            case (id, dates) =>
                var isAwake: Boolean = false
                var previousMinute: Int = -1
                val awakeMinutes: mutable.Buffer[Int] = mutable.Buffer.fill(60)(0)
                for (timestamp <- dates) {
                    isAwake = !isAwake
                    val currentMinute = timestamp.getMinuteOfDay
                    if (timestamp.getHourOfDay == 0) {
                        val currentMinute = timestamp.getMinuteOfDay
                        for (i <- previousMinute.max(0) to currentMinute)
                            awakeMinutes(i) = if (isAwake) 1 else 0
                    }
                    previousMinute = currentMinute
                }
                for (i <- previousMinute to 59)
                    awakeMinutes.updated(i, if (isAwake) 1 else 0)

                (id, awakeMinutes.toList)
        }
    }

    def solve_first(lines: List[String]): Int = {
        val guardsHours = groupDates(lines.map(line => parseLogDate(line)).sortBy(a => a._1))
        val guardsAwakeMinutes: List[(Int, List[Int])] = countAsleepMinutesPerGuard(guardsHours)

        val minutesPerGuard = guardsAwakeMinutes.groupBy(_._1).mapValues(seq => seq.reduce{(x, y) => (x._1, (x._2, y._2).zipped.map{_ + _})}).mapValues(_._2)
        val mostSleepyGuardId = minutesPerGuard.mapValues(_.sum).maxBy(_._2)._1

        minutesPerGuard(mostSleepyGuardId).zipWithIndex.maxBy(_._1)._2 * mostSleepyGuardId
    }

    def solve_second(lines: List[String]): Int = {
        val guardsHours = groupDates(lines.map(line => parseLogDate(line)).sortBy(a => a._1))
        val guardsAwakeMinutes: List[(Int, List[Int])] = countAsleepMinutesPerGuard(guardsHours)

        val minutesPerGuard = guardsAwakeMinutes.groupBy(_._1).mapValues(seq => seq.reduce{(x, y) => (x._1, (x._2, y._2).zipped.map{_ + _})}).mapValues(_._2)
        val mostSleepyGuardId = minutesPerGuard.mapValues(_.max).maxBy(_._2)._1

        minutesPerGuard(mostSleepyGuardId).zipWithIndex.maxBy(_._1)._2 * mostSleepyGuardId
    }
}
