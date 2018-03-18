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
// Main.scala
//
///////////////////////////////////////////////////////////////////////////////

package cc

import algorithms.BRKGA

/**
  * Clase principal que carga los componentes necesarios para la ejecución de
  * los algoritmos
  *
  */
object Main extends App {

	val reader = new Reader("wine.data", 1)

	reader.read()

	val launcher = new Launcher(reader, 20, 20, 999)

	/*printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))*/

	val brkga = new BRKGA(launcher, 1000, 1000, 0.2.toFloat, 0.2.toFloat, 0.6.toFloat)

}
