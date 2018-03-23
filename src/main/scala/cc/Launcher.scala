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
// Launcher.scala
//
///////////////////////////////////////////////////////////////////////////////

package cc

import scala.collection.mutable.ListBuffer
import math._

/**
  * Clase Launcher
  *
  * Base sobre la que se basarán todos los algoritmos para ejecutarse.
  * Sus principales funciones son la de almacenar la solución y definir
  * la función objetivo
  *
  * @param listData Lista con los elementos de la muestra
  * @param listLabel Lista con la etiqueta de la clase a la que pertenece
  *                  cada elemento de la muestra
  * @param constraints Número de restricciones "ML" (Must-Link) y "CL"
  *                    (cannot-link) que se deben cumplir
  * @param numClusters Número de clústeres que se deben formar
  * @param mu Valor utilizado para penalizar cuando no se cumpla una
  *           restricción
  */
class Launcher(listData: List[List[Double]], listLabel: List[Int],
			   constraints: Int = 0, numClusters: Int = 5, mu: Int = 99999999){

	private var this.listData = listData
	private var this.listLabel = listLabel

	// Listas con las restricciones.
	// - listML para "Must-link".
	// - listCL para "Cannot-link".
	// Las listas contienen las posiciones de los elementos que deben cumplir
	// esas restricciones.
	private var listML: List[List[Int]] = List[List[Int]]()
	private var listCL: List[List[Int]] = List[List[Int]]()

	private val numElements = listData.size
	private val numAttributes = listData(0).size
	private val numLabels = listLabel.toSet[Int].size

	private val this.mu = mu

	// Se asigna el número de clústeres que se deben formar
	private val this.numClusters = numClusters

	// Objeto random utilizado para crear las restricciones. Se inicializa con
	// la semilla 1818.
	private val random = new scala.util.Random(1818)

	// Número de restricciones que se deben cumplir
	private val this.constraints = constraints

	// Crea las restricciones entre los elementos
	generateConstraints()

	// Genera una solución aleatoria
	private var solution = generateRandomSolution()

	/**
	  * @constructor Guarda todos los datos de la muestra utilizando
	  *              directamente el objeto de tipo Reader
	  *
	  * @param reader Objeto de tipo Reader que contiene todos los datos
	  *               sobre los que se va a realizar las operaciones
	  * @param constraints Número de restricciones "ML" (Must-Link) y "CL"
	  *                    (cannot-link) que se deben cumplir
	  * @param numClusters Número de clústeres que se deben formar
	  * @param mu Valor utilizado para penalizar cuando no se cumpla una
	  *           restricción
	  */
	def this(reader: Reader, constraints: Int, numClusters: Int, mu: Int){
		this(reader.getListData(), reader.getListLabel(), constraints,
			numClusters, mu)
	}

	/**
	  * Devuelve la solución actual
	  *
	  * @return Vector de double con la solución
	  */
	def getSolution(): List[Double] = {
		solution
	}

	/**
	  * Devuelve el número de atributos de cada elemento
	  *
	  * @return Número de atributos
	  */
	def getNumAttributes(): Int = {
		numAttributes
	}

	/**
	  * Devuelve la etiqueta como entero que le pertenece a un elemento
	  * dependiendo del valor que tenga su posición en la solución actual
	  *
	  * @param value Número decimal que recibe
	  *
	  * @return Número entero correspondiente a la etiqueta que ha calculado
	  */
	private def getLabel(value: Double): Int = math.ceil(value*numClusters).toInt

	/**
	  * Calcula el número de restricciones que no se cumplen.
	  *
	  * @param clustered Listado con los clusters y los elementos que
	  *                  pertenecen a cada uno
	  *
	  * @return Número total de restricciones que no se cumplen
	  */
	private def getNumberNotSatisfiedConstraints(clustered: Map[Int, List[Int]]): Int = {

		var total = 0

		// Primero se comprueba la lista de restricciones Must-Link
		listML foreach{ tuple =>
			// Se filtra la lista de clústeres que se ha recibido. Si los dos
			// elementos de la tupla están en el mismo clúster, se devuelve el
			// clúster completo. Si los dos elementos no están en el mismo, se
			// tendrá un diccionario vacío.
			// El siguiente paso es comprobar que el diccionario que se
			// devuelve tiene tamaño 0, lo que significa que los dos elementos
			// de la tupla no están en el mismo clúster y, por tanto, se suma
			// 1 al resultado final.
			if(clustered.filter((t) =>
				(t._2.exists(_ == tuple(0))) && (t._2.exists(_ == tuple(1)))).size == 0)
				total += 1
		}

		listCL foreach{ tuple =>
			// Se filtra la lista de clústeres que se ha recibido. Si los dos
			// elementos de la tupla están en el mismo clúster, se devuelve el
			// clúster completo. Si los dos elementos no están en el mismo, se
			// tendrá un diccionario vacío.
			// El siguiente paso es comprobar que el diccionario que se
			// devuelve tiene tamaño 1, lo que significa que los dos elementos
			// de la tupla están en el mismo clúster y, por tanto, se suma
			// 1 al resultado final.
			if(clustered.filter((t) =>
				(t._2.exists(_ == tuple(0))) && (t._2.exists(_ == tuple(1)))).size == 1)
				total += 1
		}

		total

	}

	/**
	  * Asigna los valores del objeto de tipo Reader al conjunto de datos
	  * utilizado para realizar las operaciones
	  *
	  * @param reader Objeto de tipo Reader que contiene todos los datos
	  *               sobre los que se va a realizar las operaciones
	  */
	def setData(reader: Reader){

		var this.listData = reader.getListData()
		var this.listLabel = reader.getListLabel()

	}

	/**
	  * Genera las restricciones que deben existir entre los distintos
	  * elementos y las guarda en la lista de restricciones
	  *
	  * @param elements Número de elementos que forman cada restricción
	  */
	private def generateConstraints(elements: Int = 2){

		// Se crean dos buffers para almacenar temporalmente las restricciones
		// en su lugar correspondiente
		var ml = new ListBuffer[List[Int]]
		var cl = new ListBuffer[List[Int]]

		// Se calculan todas las restricciones posibles sin repetición
		val combinations = (1 to listData.size - 1).toList.
			combinations(elements).toList

		// Se coge solo el número de restricciones que se quiere tener.
		// Sobre cada restricciones se comprueba si sus etiquetas coinciden o
		// no. Si coinciden serán de tipo Must-Link y si no coinciden de tipo
		// Cannot-Link
		Seq.fill(constraints)(random.nextInt(combinations.size)).foreach(index =>
			if(listLabel(combinations(index)(0)) ==
					listLabel(combinations(index)(1)))
				ml += combinations(index)
			else
				cl += combinations(index)
		)

		// Se actualizan ambas listas con las restricciones
		listML = ml.toList
		listCL = cl.toList

	}

	/**
	  * Crea una solución inicial aleatoria
	  *
	  * @return Lista con valores aleatorios comprendidos en el intervalo [0,1]
	  *         y que determinan el cúster al que corresponde un elemento
	  */
	def generateRandomSolution(): List[Double] = {

		List.fill(listData.size)(random.nextDouble())

	}

	/**
	  * Evalúa la solución que recibe por parámetro y calcula el valor
	  * correspondiente.
	  *
	  * @param solution Solución que se va a comprobar
	  *
	  * @return Resultado de la evaluación con la solución recibida
	  */
	def objective(solution: List[Double]): Double = {

		// Partiendo de la solución que se recibe se calcula a que clúster
		// pertenece cada elemento. Se obtiene una lista en la que cada
		// posición corresponde a la posición del elemento e indica el clúster
		val clusters = solution.map(getLabel)

		// Se crea un diccionario con el tamaño que tiene cada clúster
		val sizes = clusters.zipWithIndex.groupBy(_._1).mapValues(_.size)

		// Se crea un diccionario donde la clave será el número de clúster y
		// el valor será la distancia total entre todos los elementos de ese
		// clúster
		var distances = Map[Int, Double]()

		// Se inicializa el diccionario de las distancias a 0
		clusters.distinct.foreach(cluster => distances += (cluster -> 0))

		// Se agrupan los elementos de la solución en sus correspondientes
		// clústeres
		val clustered = clusters.zipWithIndex.groupBy(_._1).mapValues(_.map(_._2))

		// Se agrupan los distintos elementos en sus clusters.
		clustered.foreach(
			// Se combinan todos los elementos de cada clúster sin
			// repetición
			x => x._2.combinations(2).toList.
				foreach(
					// Se calcula la distancia eucliea de los puntos
					// y se suma al total del clúster
					pair => distances = distances.
						updated(x._1, distances(x._1)+distance(listData(pair(0)),
														listData(pair(1))))
				)
			)

		// Se divide la suma total de las distancias de cada clúster por su
		// tamaño
		distances.foreach(cluster =>
			distances = distances.updated(cluster._1, distances(cluster._1)/sizes(cluster._1)))

		///////////////////////////////
		// Penalización
		//

		// Número de restricciones que no se cumplen
		val infeasibility = getNumberNotSatisfiedConstraints(clustered)

		// fitness = Z + (mu * n * infeasibility)
		distances.map(cluster => cluster._2).sum + (mu * numElements * infeasibility)

		//
		///////////////////////////////

	}

	/**
	  * Calcula la distancia entre dos elementos
	  *
	  * @param element1 Primer objeto
	  * @param element2 Segundo objeto
	  *
	  * @return Distancia entre los dos elementos
	  */
	private def distance(element1: List[Double], element2: List[Double]): Double = {

		sqrt((element1 zip element2).map { case (x,y) => pow(y - x, 2) }.sum)

	}

}
