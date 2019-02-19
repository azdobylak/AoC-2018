package day07

import java.net.URL

import scala.io.Source
import scala.collection.mutable


case class Worker(var sleepTime: Int,
                  var task: Option[Char] = None,
                  var secondsToFinishTask: Int = 0) {

    def finishTask(): Unit = this.task = None

    def setTask(task: Char): Unit = {
        this.task = Some(task)
        this.secondsToFinishTask = this.sleepTime
    }

    def work(): Unit = this.secondsToFinishTask -= 1

    def isBusy: Boolean = this.secondsToFinishTask > 0
    def hasFinished: Boolean = this.task.isEmpty
}


object Day07 {
    val input: URL = day07.Day07.getClass.getResource("input")
    val lines: List[String] = Source.fromURL(input).getLines().toList

    def main(args: Array[String]): Unit = {
        println(solve_first(lines))
        //println(solve_second(lines))
    }

    def parseInput(line: String): (String, String) = {
        val pattern = "Step ([a-zA-Z]) must be finished before step ([a-zA-Z]) can begin.".r
        val matches = pattern.findAllIn(line)
        (matches.group(1), matches.group(2))
    }

    def traversePseudoTree(charPairs: List[(String, String)], perCharSecondsOverhead:Int,  workersNum: Int = 1): String = {
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

        val workers: List[Worker] = List.fill[Worker](workersNum)(Worker(perCharSecondsOverhead))

        var result: String = ""

        do{
            for(worker <- workers)
            {
                if (worker.isBusy)
                    worker.work()
                else
                    worker.task match{
                        case Some(c: Char) => result += c; worker.finishTask()
                        case None => worker.setTask(traverse(charPairs, result))
                    }
            }
        } while(availableTasks.nonEmpty || workers.map(!_.hasFinished).exists(identity))

        result
    }

    def solve_first(lines: List[String]): String = {
        val charPairs: List[(String, String)]  = lines.map(l => parseInput(l))
        traversePseudoTree(charPairs, 0, 1)
    }

    def solve_second(lines: List[String]): Int = {
        0
    }
}