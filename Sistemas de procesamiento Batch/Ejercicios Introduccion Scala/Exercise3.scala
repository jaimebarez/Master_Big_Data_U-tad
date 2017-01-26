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


/**
  * Genere la siguiente jerarquía de clases (Múltiples herencias)
  * Implemente un método para filtrar los vehículos de tipo AirVehicle
  * Utilize pattern matching
  * Implemente un método para obtener la velocidad media
  */
object Exercise3 extends App {
  //Lista de vehículos
  val vehicles: List[Vehicle] = List(
    new Bus("EMT", 120),
    new Car("Seat", 220),
    new Helicopter("Apache", 200, 350),
    new Plane("Airbus", 800, 1000))

  //Soluciones ejercicio
  val ex = new Exercise3()
  println(s"Ground Vehicles: ${ex.filterGroundVehicles(vehicles)}")
  println(s"Velocidad media: ${ex.getAverageMaxSpeed(vehicles)}")
}

//Clases de "negocio":

abstract class Vehicle {
  val name: String
}

abstract case class GroundVehicle(name: String, maxSpeed: Double) extends Vehicle

abstract case class AirVehicle(name: String, airspeed: Double, maxSpeed: Double) extends Vehicle

class Bus(name: String, maxSpeed: Double) extends GroundVehicle(name, maxSpeed)

class Car(name: String, maxSpeed: Double) extends GroundVehicle(name, maxSpeed)

class Helicopter(name: String, airspeed: Double, maxSpeed: Double) extends AirVehicle(name, airspeed, maxSpeed)

class Plane(name: String, airspeed: Double, maxSpeed: Double) extends AirVehicle(name, airspeed, maxSpeed)

/**
  * Class that provides the implementation for the solution of the exercise 3.
  */
class Exercise3 {
  /**
    * Dada una lista de vehículos, devuelve una lista de los que son GroundVehicle
    *
    * @param vehicles
    * @return
    */
  def filterGroundVehicles(vehicles: List[Vehicle]): List[GroundVehicle] = {

    vehicles.flatMap(v => {
      v match {
        case GroundVehicle(_, _) => List[GroundVehicle](v.asInstanceOf[GroundVehicle])
        case _ => Nil
      }
    })

  }

  /**
    * Dada una lista de vehículos, nos devuelve la velocidad media
    *
    * @param vehicles
    * @return
    */
  def getAverageMaxSpeed(vehicles: List[Vehicle]): Double = {
    vehicles.foldLeft(0D)((acc, cur) => {
      acc + {
        cur match {
          case GroundVehicle(_, max_speed) => max_speed
          case AirVehicle(_, _, max_speed) => max_speed
        }
      } / vehicles.length
    })
  }

}
