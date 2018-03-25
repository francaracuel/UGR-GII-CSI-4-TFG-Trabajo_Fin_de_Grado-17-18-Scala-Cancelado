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
// Reader.scala
//
///////////////////////////////////////////////////////////////////////////////

package cc

import scala.collection.mutable.ListBuffer

/**
  * Clase Reader
  *
  * Carga los datos de un fichero del que recibe el nombre.
  * Ofrece métodos para su posterior recuperación
  *
  * @param filename Nombre del fichero que contiene los datos
  * @param labelColumn Número de columna donde se encuentra la etiqueta a la
  *                    que pertenece cada dato:
  *                    - [1] la etiqueta aparece en la primera columna
  *                    - (0) el fichero de datos no contiene la etiqueta
  *                    - (-1) la etiqueta aparece en la última columna
  */
class Reader(filename: String, labelColumn: Int = -1){

	// Se guarda la ruta absoluta hasta la raíz del proyecto
	private val path = new java.io.File(".").getCanonicalPath+"/data/"

	// Se guardar el nombre del fichero
	private val this.filename = filename

	// Se crea la ruta completa incluyendo al fichero
	private var file = path+filename

	// Guarda el número de columna donde se encuentra la etiqueta a la que
	// pertenece cada dato, que será separada de los propios datos.
	private var this.labelColumn = labelColumn

	// Lista con los datos que se cargan
	private var listData: List[List[Double]] = _

	// Lista con la clase o etiqueta a la que pertenece cada dato
	private var listLabel: List[Int] = _

	// Número de muestras
	private var numElements = 0

	// Número de atributos
	private var numAttributes = 0

	/**
	  * Devuelve la lista con todos los datos de la muestra
	  *
	  * @return Lista con los datos de la muestra
	  */
	def getListData: List[List[Double]] = {
		listData
	}

	/**
	  * Devuelve la lista con todas las etiquetas de los datos
	  *
	  * @return Lista con las etiquetas de los datos
	  */
	def getListLabel: List[Int] = {
		listLabel
	}

	/**
	  * Devuelve el número de elementos que tiene la muestra
	  *
	  * @return Número de elementos que tiene la muestra
	  */
	def getNumElements: Int = {
		numElements
	}

	/**
	  * Devuelve el número de atributos que tiene cada elemento de la muestra
	  *
	  * @return Número de atributos que tiene cada elemento de la muestra
	  */
	def getNumAttributes: Int = {
		numAttributes
	}

	/**
	  * Dependiendo del origen de los datos, se devuelve un identificador para
	  * cada una de las clases de la muestra
	  *
	  * @param label Etiqueta de la clase a la que pertenece un elemento
	  *
	  * @return Valor entero
	  */
	def getLabelValue(label: String = "") : Int = {

		// Dependiendo del fichero de datos, si las etiquetas no se pueden
		// convertir a enteros, se realiza manualmente
		filename match {

		 	case "iris.data" => label match {

				case "Iris-setosa" => 1

				case "Iris-versicolor" => 2

				case "Iris-virginica" => 3

			}

			case default => label.toInt

		}

	}

	/**
	  * Lee los datos de un fichero y guarda dos listas, una con los propios
	  * datos y otra con las etiquetas
	  *
	  */
	def read(){

		// Lista con los datos que se cargan
		var listData = new ListBuffer[List[Double]]()

		// Lista con la clase o etiqueta a la que pertenece cada dato
		var listLabel = new ListBuffer[Int]()

		// Se guarda en memoria el fichero
		val bufferedSource = io.Source.fromFile(file)

		// Se recorre línea a línea el fichero
		for(line <- bufferedSource.getLines()){

			// Se crea un array con los elementos de cada línea que se
			// encuentran separados por comas
			val l = line.split(",").map(_.trim)

			//listData += l.map(_.toDouble).dropRight(1).toList

			// Dependiendo del valor de "labelColumn" se obtiene la etiqueta
			// de una columna u otra
			labelColumn match{

				// Si es -1, la columna que tiene la etiqueta es la última
				case -1 =>

					// Se añade la fila convirtiéndola a double
					listData += l.dropRight(1).map(_.toDouble).toList

					// Se añade la etiqueta a la que pertenece el dato
					listLabel += getLabelValue(l(l.length-1))

				// Si es 1, la columna que tiene la etiqueta es la primera
				case 1 =>

					// Se añade la fila convirtiéndola a double
					listData += l.drop(1).map(_.toDouble).toList

					// Se añade la etiqueta a la que pertenece el dato
					listLabel += getLabelValue(l(0))

				// Si es 0, no existe la etiqueta
				case 0 =>

					// Se añade la fila convirtiéndola a double
					listData += l.map(_.toDouble).toList

					// Se añade el vacío
					listLabel += getLabelValue()

			}

		}

		// Se guarda el número de muestras
		numElements = listData.size

		// Se guarda el número de atributos
		numAttributes = listData.head.size

		// Para trabajar con los datos es necesario convertirlos a lista
		this.listData = listData.toList
		this.listLabel = listLabel.toList

		// Se cierra el flujo del fichero
		bufferedSource.close()

	}

	/**
	  * Sobreescribe el método toString para poder mostrar en formato de tabla
	  * los datos de la muestra
	  */
	override def toString: String = {

		// Se inicializa el string vacío
		var res = ""

		// Se recorre cada muestra
		for(i <- 0 until numElements){

			// Se recorre cada atributo
			for(j <- 0 until numAttributes){

				// Se concatena cada atributo al resultado final
				res += listData(i)(j) + ", "

			}

			// Se concatena a la muestra la etiqueta de la clase a la que
			// pertenece
			res += listLabel(i) + "\n"

		}

		// Se devuelve el resultado final
		res

	}

}
