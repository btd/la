package com.github.la

import La._

object Approximation {
	
	def weight(x: Double, y: Double): Double = gauss(x) * gauss(y)

	def norm(x1: Matrix, x2: Matrix): Matrix = sqrt(x1 ** x1 + x2 ** x2)

	def kernel(m: Matrix): Matrix = {
		val a = 0.05
		exp(-abs(m)/a)**(1 + abs(m)/ a)
	}

	def normalizedWeight(groups: Seq[Area], x: Vector, y: Vector, normP: Double = 2.0):Seq[Map[Int, Double]] = {
		require(normP > 0)

		for{
			i <- x.indexes
			xI = x(i)
			yI = y(i)
		} yield {
			val weightsMap = groups.zipWithIndex.withFilter(_._1.in_?(xI, yI)).map { p =>
				val (scaledX, scaledY) = p._1.scale(xI, yI)
				p._2 -> weight(scaledX, scaledY) 
			}.toMap
				
			val areaNorm = Vector(weightsMap.values).norm(normP)
			weightsMap.mapValues(_ / areaNorm)
		}
	}

	def PU(gridX: Vector, gridY: Vector, cpX: Vector, cpY: Vector, cpZ: Vector, groups: Seq[Area]): Vector = {
		var values = Vector.zeros(gridX.size)

		val weightsAll = normalizedWeight(groups, gridX, gridY, 1.0)

		for((group, groupIdx) <- groups.zipWithIndex) {
			val idx = group.in(cpX, cpY)

			val groupedCpX = cpX(idx)
			val groupedCpY = cpY(idx)
			val groupedCpZ = cpZ(idx)

			val B = groupedCpZ - groupedCpZ.mean asCol

			val (xi, xj) = Matrix.meshgrid(groupedCpX, groupedCpX)
			val (yi, yj) = Matrix.meshgrid(groupedCpY, groupedCpY)
			val A = kernel(norm((xi - xj), (yi - yj)))

			val coeff = new CholeskyDecomposition(A).solve(B)

			val idxGrid = group.in(gridX, gridY)
			val groupedGridX = gridX(idxGrid)
			val groupedGridY = gridY(idxGrid)

			val weights = Vector(idxGrid.map(pointIndex => weightsAll(pointIndex)(groupIdx)))

			val (gridXi, gridXj) = Matrix.meshgrid(groupedGridX, groupedCpX)
			val (gridYi, gridYj) = Matrix.meshgrid(groupedGridY, groupedCpY)

			val lll = sum(kernel(norm(gridXi - gridXj, gridYi - gridYj)) * coeff.asCol) + groupedCpZ.mean

			values(idxGrid) = values(idxGrid) + weights ** lll

		}
		values
	}

	def PUquad(gridX: Vector, gridY: Vector, cpX: Vector, cpY: Vector, cpZ: Vector, groups: Seq[Area]): Vector = {
		var values = Vector.zeros(gridX.size)

		val weightsAll = normalizedWeight(groups, gridX, gridY, 2.0)
		val weightsCpAll = normalizedWeight(groups, cpX, cpY, 2.0)

		for((group, groupIdx) <- groups.zipWithIndex) {
			val idx = group.in(cpX, cpY)

			val weightsCp = Vector(idx.map(pointIndex => weightsCpAll(pointIndex)(groupIdx)))

			val groupedCpX = cpX(idx)
			val groupedCpY = cpY(idx)
			val groupedCpZ = cpZ(idx)

			val B = (groupedCpZ - groupedCpZ.mean) ** weightsCp asCol

			val (xi, xj) = Matrix.meshgrid(groupedCpX, groupedCpX)
			val (yi, yj) = Matrix.meshgrid(groupedCpY, groupedCpY)
			val A = kernel(norm((xi - xj), (yi - yj)))

			val coeff = new CholeskyDecomposition(A).solve(B)

			val idxGrid = group.in(gridX, gridY)
			val groupedGridX = gridX(idxGrid)
			val groupedGridY = gridY(idxGrid)

			val weightsGrid = Vector(idxGrid.map(pointIndex => weightsAll(pointIndex)(groupIdx)))

			val (gridXi, gridXj) = Matrix.meshgrid(groupedGridX, groupedCpX)
			val (gridYi, gridYj) = Matrix.meshgrid(groupedGridY, groupedCpY)

			val lll = sum(kernel(norm(gridXi - gridXj, gridYi - gridYj)) * coeff.asCol) + groupedCpZ.mean * weightsGrid

			values(idxGrid) = values(idxGrid) + weightsGrid ** lll

		}
		values
	}

	def RBF(gridX: Vector, gridY: Vector, cpX: Vector, cpY: Vector, cpZ: Vector): Vector = {
		val (xi, xj) = Matrix.meshgrid(cpX, cpX)
		val (yi, yj) = Matrix.meshgrid(cpY, cpY)

		val coeff = new CholeskyDecomposition(kernel(norm((xi - xj), (yi - yj)))).solve(cpZ asCol)

		val (gridXi, gridXj) = Matrix.meshgrid(gridX, cpX)
		val (gridYi, gridYj) = Matrix.meshgrid(gridY, cpY)

		sum(kernel(norm(gridXi - gridXj, gridYi - gridYj)) * coeff.asCol)
	}
}