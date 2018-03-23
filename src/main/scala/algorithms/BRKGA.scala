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

import scala.collection.mutable.ListBuffer

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
  * @param pElite Porcentaje de cromosomas que participarán en la siguiente
  *              generación siendo de la actual
  * @param pMutants Porcentaje de cromosomas que mutarán en cada generación
  * @param pInherit Probabilidad de que un alelo sea heredado del padre
  */
class BRKGA(launcher: Launcher, numPopulation: Int, numGenerations: Int,
			pElite: Float, pMutants: Float, pInherit: Float) {

	private val this.launcher: Launcher = launcher

	private val this.numPopulation: Int = numPopulation

	private val this.numGenerations: Int = numGenerations

	private val this.pElite: Float = pElite

	// Se calcula la cantidad de cromosomas que se consideran élite en cada
	// generación
	private val numElite: Int = (numPopulation*pElite).toInt

	// Se calcula la cantidad de cromosomas que no se condideran élite
	private val numNotElite: Int = numPopulation - numElite

	private val this.pMutants: Float = pMutants

	// Se calcula la cantidad de cromosomas que serán mutaciones
	private val numMutants: Int = (numPopulation*pMutants).toInt

	private val this.pInherit: Float = pInherit

	// Se calcula la cantidad de cromosomas restantes de la élite y mutaciones
	private val numRemainder: Int = numPopulation - numElite - numMutants

	// Contador utilizado para saber cuántas veces se ha creado una nueva
	// generación
	private var generationCounter: Int = 0

	// Se crea una población inicial de cromosomas con valores aleatorios
	private var population: List[List[Double]] = List.
						fill(numPopulation)(launcher.generateRandomSolution())

	// Lista que tendrá el resultado de evaluar cada cromosoma
	private var fitnesses: List[Double] = List()

	// Se decodifica y calcula el valor de toda la población
	decodePopulation()

	// Lista ordenada con los índices de los cromosomas de mejor a peor
	private var sortedPopulation: List[Int] = List()

	// Objeto random utilizado para crear las restricciones. Se inicializa con
	// la semilla 1818.
	private val random = new scala.util.Random(1818)

	// Número de atributos de los cromosomas
	private val numAttributesChromosome: Int = launcher.getNumAttributes

	/**
	  * Decodifica y calcula el valor que le corresponde a cada cromosoma de la
	  * población
	  */
	private def decodePopulation(): Unit ={
		fitnesses = population.map(launcher.objective(_))
	}

	/**
	  * Inicia la ejecución del algoritmo
	  *
	  * @return Una tupla con la mejor solución y su valor correspondiente
	  */
	def run(): (List[Double], Double) ={

		// Se continuará con el proceso hasta que se hayan creado todas las
		// generaciones que se esperan
		while(generationCounter < numGenerations){

			// Se obtiene una lista ordenada con los índices de mejor a peor
			val (values, indexes) = fitnesses.zipWithIndex.sorted.unzip

			// Se guarda la lista de los índices de los cromosomas ordenados
			sortedPopulation = indexes


			//println(generationCounter)
			Console.printf("Generación %d: %.0f\n", generationCounter, fitnesses(sortedPopulation(0)))
			//println(random.nextInt(numElite))
			//println(random.nextInt(numNotElite)+numElite)



			// Se guarda la élite de la generación actual
			val elite = sortedPopulation.take(numElite).map(population(_))

			// Se crean los mutantes de la nueva generación
			val mutants = List.fill(numMutants)(launcher.generateRandomSolution())

			// Los cromosomas restantes vienen dados por el cruce entre
			// miembros de la élite y el resto de la población actual
			val offspring = List.fill(numRemainder)(crossover(
						population(sortedPopulation(random.nextInt(numElite))),
						population(sortedPopulation(random.nextInt(numNotElite)+numElite))))

			// Se crea la lista que contendrá la nueva generación. Contendrá
			// la élite de la actual generación, una serie de mutaciones
			// (aleatorios) y el restante serán cruces de la élite de la
			// población actual con el resto
			population  = elite ::: mutants ::: offspring

			// Se decodifica (se aplica la función objetivo a cada cromosoma)
			// la población completa
			decodePopulation()

			// Se aumenta el contador de generaciones
			generationCounter += 1

		}

		// Se devuelve el mejor cromosoma y su valor
		(population(sortedPopulation(0)), fitnesses(sortedPopulation(0)))

	}

	/**
	  * Realiza la operación de cruce entre dos cromosomas
	  *
	  * @param chromosome1 Primer cromosoma
	  * @param chromosome2 Segundo cromosoma
	  *
	  * @return Cromosoma resultante de realizar el cruce
	  */
	def crossover(chromosome1: List[Double], chromosome2: List[Double]): List[Double] = {

		// Se crea una tupla para poder hacer el cruce de manera más cómoda
		val chromosomes = List(chromosome1, chromosome2)

		// Se realiza el cruce eligiendo con probabilidad 0.5 los alelos de
		// uno u otro cromosoma
		(0 to numAttributesChromosome).toList.map(chromosomes(random.nextInt(2))(_))

	}

}
