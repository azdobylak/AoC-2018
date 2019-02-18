package day07

import java.net.URL

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.mutable


object Day07 {
    val input: URL = day07.Day07.getClass.getResource("input")
    val lines: List[String] = Source.fromURL(input).getLines().toList

    def main(args: Array[String]): Unit = {
        println(solve_first(lines))
        println(solve_second(lines))
    }

    def parseInput(line: String): (String, String) = {
        val pattern = "Step ([a-zA-Z]) must be finished before step ([a-zA-Z]) can begin.".r
        val matches = pattern.findAllIn(line)
        (matches.group(1), matches.group(2))
    }

    def traversePseudoTree(charPairs: List[(String, String)]): String = {
        var availableTasks = mutable.SortedSet[String](charPairs.head._1)

        def traverse(tasksMap: List[(String, String)], tasksOrder: String): Char = {
            val tasksLeft = tasksMap.filterNot(tasksOrder contains _._1)
            val tasksWaitingForPrerequisite: List[String] =
                tasksLeft.collect{ case pair if !(tasksOrder contains pair._1) => pair._2 }
            val tasksPairsInQueue = tasksLeft.filter(pair => !tasksWaitingForPrerequisite.contains(pair._1))

            availableTasks ++= tasksPairsInQueue.map(_._1)

            val task = availableTasks.head
            availableTasks = availableTasks.tail
            val foundTasks = tasksLeft.filter(_._1 == task)
            val otherTasks = tasksLeft.filter(_._1 != task)

            val acc = tasksOrder + task

            for(taskPair <- foundTasks) {
                val availableTask = taskPair._2
                val arePrerequisitesDone: Boolean = tasksLeft.filter(_._2 == availableTask).map(acc contains _._1).forall(identity)
                if(arePrerequisitesDone && !(tasksOrder contains availableTask))
                    availableTasks += availableTask
            }
            task.charAt(0)
        }

        var result: String = ""

        do{
            val char: Char = traverse(charPairs, result)
            result += char
        } while(availableTasks.nonEmpty)

        result
    }

    def solve_first(lines: List[String]): String = {
        val charPairs: List[(String, String)]  = lines.map(l => parseInput(l))
        traversePseudoTree(charPairs)
    }

    def solve_second(lines: List[String]): Int = {
        0
    }
}
