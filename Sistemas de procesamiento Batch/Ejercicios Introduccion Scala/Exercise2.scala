/*
 * Copyright (c) 2016 Jaime Bárez.
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

import scala.collection.immutable.{ListMap}
import scala.util.Random

/**
  * Defina una case class Persona con atributos name, age, y email
  * Genere automáticamente 50 instancias de Persona
  * -Pueden tener nombres consecutivos
  * -Genere una función que agrupe todas las personas con una misma edad en
  * una estructura tipo: Map[Int, List[Person]]
  * -Ofrezca dos implementaciones (al menos una funcional con foldLeft)
  */
object Exercise2 extends App {
  /**
    * Devuelve un String de longitud dada con caracteres a-z (pueden ser mayúsculas y minúsculas)
    * Inspirado por http://alvinalexander.com/scala/creating-random-strings-in-scala
    *
    * @param length
    * @return
    */
  def randomName(length: Int): String = {
    val chars = ('a' to 'z') ++ ('A' to 'Z')
    randomStringFromCharList(length, chars)
  }

  /**
    * Devuelve un String de longitud dada con caracteres dados
    *
    * @param length
    * @param chars
    * @return
    */
  def randomStringFromCharList(length: Int, chars: Seq[Char]): String = {
    (0 until length)
      .foldLeft(
        new StringBuilder()
      )(
        (stb, _) => stb.append(
          chars(Random.nextInt(chars.length))
        ))
      .toString()
  }

  val num_personas = 50
  val edad_max = 100
  val tamano_nombre = 10

  //Creamos una lista de personas
  val personas = (0 until num_personas).map(_ => {
    val name = randomName(tamano_nombre)

    new Person(name, Random.nextInt(edad_max), s"${name}@gmail.com")
  })

  val separator = System.getProperty("line.separator")

  //Imprimimos la lista de personas
  println("Lista de personas:")
  println(personas.mkString(separator))

  //Separador de salida de consola
  println((0 until 20).map(_ => "-").mkString(" "))

  //Imprimimos la lista de personas agrupadas por edad de forma ordenada
  val ex = new Exercise2()
  println(ex.groupByAgeSorted(personas.to[List]).mkString(separator))
}

/**
  * Representa una persona
  *
  * @param name
  * @param age
  * @param email
  */
class Person(val name: String, val age: Int, email: String) {

  override def toString: String = {
    s"Usuario ${this.name} con mail ${this.email}, edad ${this.age}"
  }
}

/**
  * Class that provides the implementation for the solution of the exercise 2.
  */
class Exercise2 {
  /**
    * Agrupa las personas de la misma edad en una estructura de mapa
    *
    * @param persons
    * @return
    */
  def groupByAge(persons: List[Person]): Map[Int, List[Person]] = {
    persons.groupBy(_.age.toInt)
  }

  /**
    * Agrupa las personas de la misma edad en una estructura de mapa ordenada
    *
    * @param persons
    * @return
    */
  def groupByAgeSorted(persons: List[Person]): Map[Int, List[Person]] = {
    groupByAge(persons).toSeq.sortBy(_._1)
      .foldLeft(ListMap.empty[Int, List[Person]]
      )(
        (ac, cur) => {
          ac + cur
        })
  }
}

