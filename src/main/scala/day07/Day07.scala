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
        @annotation.tailrec
        def traverse(tasksMap: List[(String, String)], availableTasks: mutable.SortedSet[String], tasksOrder: String): String = {
            val tasksWaitingForPrerequisite: List[String] =
                tasksMap.collect{ case pair if !(tasksOrder contains pair._1) => pair._2 }
            val tasksPairsInQueue = tasksMap.filter(pair => !tasksWaitingForPrerequisite.contains(pair._1))

            availableTasks ++= tasksPairsInQueue.map(_._1)

            if(availableTasks.isEmpty)
                return tasksOrder

            val task = availableTasks.head
            val availableTasksTail = availableTasks.tail
            val foundTasks = tasksMap.filter(_._1 == task)
            val otherTasks = tasksMap.filter(_._1 != task)

            val acc = tasksOrder + task

            for(taskPair <- foundTasks) {
                val availableTask = taskPair._2
                val arePrerequisitesDone: Boolean = tasksMap.filter(_._2 == availableTask).map(acc contains _._1).forall(identity)
                if(arePrerequisitesDone && !(tasksOrder contains availableTask))
                    availableTasksTail += availableTask
            }

            traverse(otherTasks, availableTasksTail, acc)
        }
        traverse(charPairs, mutable.SortedSet[String](charPairs.head._1), "")
    }

    def solve_first(lines: List[String]): String = {
        val charPairs: List[(String, String)]  = lines.map(l => parseInput(l))
        traversePseudoTree(charPairs)
    }

    def solve_second(lines: List[String]): Int = {
        0
    }
}
