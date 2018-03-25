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
// SLS.scala
//
///////////////////////////////////////////////////////////////////////////////

package algorithms

import cc.Launcher

/**
  * Clase SLS (Shift Local Search)
  *
  * Gestiona todo el proceso necesario para llevar a cabo la búsqueda local
  * en una solución
  *
  * @param launcher Clase que contiene los datos, decodifica la solución y
  *                 calcula su valor con la función objetivo
  * @param threshold Umbral que determina lo que debe mejorar una solución para
  *                  ser considerada mejor
  * @param maxIter Número máximo de iteraciones que tiene permitido el
  *                algoritmo ejecutar
  *
  */
class SLS(launcher: Launcher, threshold: Double = 1, maxIter: Int = 1) {

	private val this.launcher: Launcher = launcher

	private var solution: List[Double] = _

	// Umbral para determinar lo que debe mejorar una solución para ser
	// considerada mejor
	private val this.threshold = threshold

	// Número máximo de iteraciones
	private val this.maxIter = maxIter

	// Se inicializa el objeto generador con la semilla 1818.
	//private val random = new scala.util.Random(1818)

	/**
	  * Asigna los datos necesarios para realizar la búsqueda local
	  *
	  * @param solution Solución sobre la que se quiere ejecutar el algoritmo
	  */
	def setData(solution: List[Double]): Unit ={

		this.solution = solution

	}

	/**
	  * Ejecuta el algoritmo de búsqueda local
	  *
	  * @return Devuelve la mejor solución y su valor
	  *
	  */
	def run(): (Map[Int, List[Int]], Double) = {

		// La solución que se recibe no está decodificada en clústeres, por lo
		// que se decodifica
		var bestSolution = launcher.decode(solution)
		var bestFitness = launcher.objective(bestSolution)

		val currentSolution = bestSolution

		var auxSolution = bestSolution
		var auxFitness = bestFitness

		// Para necesitar menos cálculo se obtiene una lista con los clústeres
		val clusters = bestSolution.keys

		var i = 0

		// Flag que controla si se mejora la solución
		var improve = false

		// Se ejecuta el algoritmo mientras que se mejore la mejor solución
		do{

			// Se inicializa el flag que permite saber si se ha mejorado
			improve = false

			// Para poder recorrer primero todos los objetos y después los
			// respectivos clústeres es necesario invertir el diccionario
			currentSolution.map(_.swap).foreach(elements => {

				// Se recorren todos los elementos. element tendrá cada
				// objeto de la solución
				elements._1.foreach(element => {

					//println("Objeto "+i+": "+element)

					// Se guarda el clúster para guardar el nuevo si es mejorada
					// la solución
					var currentCluster = elements._2

					// Se recorre cada clúster
					clusters.foreach(cluster => {

						// Se copia la mejor solución hasta el momento
						auxSolution = bestSolution

						// Se elimina el objeto de su clúster
						auxSolution = auxSolution.updated(currentCluster, auxSolution(currentCluster) diff List(element))

						// Se añade el objeto a otro clúster
						auxSolution = auxSolution.updated(cluster, List(element) ::: auxSolution(cluster))

						auxFitness = launcher.objective(auxSolution)

						// Si el nuevo fitness mejora un cierto umbral al
						// mejor, se actualiza
						if (bestFitness - auxFitness >= threshold) {

							/*println("Current solution: ", currentSolution)
							println("Element: ", element)
							println("Old cluster: ", currentCluster)
							println("New cluster: ", cluster)
							println("New solution: ", currentSolution)
							println("Best fitness: ", bestFitness)
							println("Current fitness: ", auxFitness)
							println("__________________")*/

							bestSolution = auxSolution
							bestFitness = auxFitness

							currentCluster = cluster

							improve = true

						}

					})

				})

			})

			//println("Best fitness: ", bestFitness)

			/*if(improve){
				println("Ha mejorado")
			} else{
				println("No ha mejorado")
			}*/

			i += 1

		}while(improve && i < maxIter)

		printf("%.0f\n", bestFitness)

		(bestSolution, bestFitness)

	}

	/**
	  * Ejecuta el algoritmo de búsqueda local
	  *
	  * @param solution Solución sobre la que se quiere ejecutar el algoritmo
	  *
	  * @return Devuelve la mejor solución y su valor
	  */
	def run(solution: List[Double]): (Map[Int, List[Int]], Double) = {

		setData(solution)

		run()

	}

}
