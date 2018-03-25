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

	val constraints = 20
	val clusters = 10
	val mu = 9999

	val launcher = new Launcher(reader, constraints, clusters, mu)

	//printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	/*printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))
	printf("The value is %.0f\n", launcher.objective(launcher.generateRandomSolution()))*/

	var t0 = System.currentTimeMillis()

	val population = 100
	val generations = 200
	val pElite = 0.2.toFloat
	val pMutants = 0.2.toFloat
	val pInherit = 0.6.toFloat

	val brkga = new BRKGA(launcher, population, generations, pElite, pMutants, pInherit, ls = false)

	val (bestSolutionBRKGA, bestValueBRKGA) = brkga.run()

	var t1 = System.currentTimeMillis()

	Console.printf("%.0f\n", bestValueBRKGA)

	println("Tiempo total (BRKGA): "+(t1-t0)/1000+" segundos")

}
