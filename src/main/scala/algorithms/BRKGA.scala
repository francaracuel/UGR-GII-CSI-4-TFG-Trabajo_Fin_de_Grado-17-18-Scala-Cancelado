///////////////////////////////////////////////////////////////////////////////
//
// Francisco Javier Caracuel Beltrán
//
// 4º GII - Computación y Sistemas Inteligentes (CSI)
//
// ETSIIT - UGR - 2017/2018
//
// TFG
//
// BRKGA.scala
//
///////////////////////////////////////////////////////////////////////////////

package algorithms

import cc.{Launcher, Reader}

/**
  * Clase BRKGA
  *
  * Gestiona todo el proceso necesario para llevar a cabo el algoritmo
  * genético de clave aleatoria sesgada con búsqueda local.
  *
  * @param launcher Clase que contiene los datos, decodifica la solución y
  *                 calcula su valor con la función objetivo
  * @param numPopulation Número de cromosomas de la población
  * @param numGenerations Número de generaciones que se permite generar
  * @param elite Porcentaje de cromosomas que participarán en la siguiente
  *              generación siendo de la actual
  * @param mutants Porcentaje de cromosomas que mutarán en cada generación
  * @param pInherit Probabilidad de que un alelo sea heredado del padre
  */
class BRKGA(launcher: Launcher, numPopulation: Int, numGenerations: Int,
			elite: Float, mutants: Float, pInherit: Float) {

	private val this.launcher = launcher

	private var generationCounter = 0

	private var population: List[List[Double]] = List.fill(numPopulation)(launcher.generateRandomSolution())

	private var fitnesses: List[Double] = List()

	decodePopulation()

	Console.println(fitnesses)



	private def decodePopulation(): Unit ={

		fitnesses = population.map(launcher.objective(_))

	}

}
