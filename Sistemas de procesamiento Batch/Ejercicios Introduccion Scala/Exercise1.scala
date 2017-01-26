/*
 * Copyright (c) 2015 Daniel Higuero.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.scala.exercises

/**
  *
  */
object Exercise1 extends App {
  val e1 = Exercise1()


  val list = List(1, 2, 3, 4, 5)
  val r1 = e1.processList(list, transformToPepitoString)
  println(r1)

  val r2 = e1.processList(list, square)
  println(r2)

  def apply(): Exercise1 = new Exercise1()

  def square(int: Int): Int = {
    Math.pow(int, 2).toInt
  }

  def transformToPepitoString(int: Int): String = {
    "pepito" + int.toString()
  }

}

/**
  * Class that provides the implementation for the solution of the exercise 1.
  */
class Exercise1 {
  def processList[T, B](list: List[T], f: (T) => B): Map[T, B] = {
    return list.foldLeft(Map.empty[T, B])((acc, cur) => acc + {
      cur -> f(cur)
    })
    //Ineficiente para el caso, pero para aprender el "++"
    //list.foldLeft(Map.empty[T, B])((acc, cur) => acc ++ Map[T, B]((cur, f(cur))))
  }

  def processList2[T, B](list: List[T], f: (T) => B): Map[T, B] = {

    var map: Map[T, B] = Map.empty[T, B]

    list.foreach(v => {
      map += v -> f(v)
    })
    return map
  }


}
