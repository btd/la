package com.github.la

import La._

object Approximation {
	
	def hann2D(x: Double, y: Double): Double = hann(x) * hann(y)


	def norm(x1: Matrix, x2: Matrix): Matrix = sqrt(x1 ** x1 + x2 ** x2)

	def kernel(m: Matrix): Matrix = {
		val a = 0.04
		//exp(-abs(m)/a)**(1 + abs(m)/ a)
		sqrt(m ** m + a * a)
	}

	def normalizedWeight(groups: Seq[Area], x: Vector, y: Vector, weight: (Double, Double) => Double, normP: Double = 2.0):Seq[Map[Int, Double]] = {
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

		val weightsAll = normalizedWeight(groups, gridX, gridY, hann2D _, 1.0)

		for((group, groupIdx) <- groups.zipWithIndex) {
			val idx = group.in(cpX, cpY)

			val groupedCpX = cpX(idx)
			val groupedCpY = cpY(idx)
			val groupedCpZ = cpZ(idx)

			val B = groupedCpZ - groupedCpZ.mean asCol

			val (xi, xj) = Matrix.meshgrid(groupedCpX, groupedCpX)
			val (yi, yj) = Matrix.meshgrid(groupedCpY, groupedCpY)
			val A = kernel(norm((xi - xj), (yi - yj)))

			val coeff = new LUDecomposition(A).solve(B)

			val idxGrid = group.in(gridX, gridY)
			val groupedGridX = gridX(idxGrid)
			val groupedGridY = gridY(idxGrid)

			val weights = Vector(idxGrid.map(pointIndex => weightsAll(pointIndex)(groupIdx)))

			val (gridXi, gridXj) = Matrix.meshgrid(groupedGridX, groupedCpX)
			val (gridYi, gridYj) = Matrix.meshgrid(groupedGridY, groupedCpY)

			val lll = sum(kernel(norm(gridXi - gridXj, gridYi - gridYj)) ** coeff.asCol) + groupedCpZ.mean

			values(idxGrid) = values(idxGrid) + weights ** lll

		}
		values
	}

	def PUquad(gridX: Vector, gridY: Vector, cpX: Vector, cpY: Vector, cpZ: Vector, groups: Seq[Area]): Vector = {
		var values = Vector.zeros(gridX.size)

		def w(x: Double, y: Double) = math.sqrt(hann2D(x, y))

		val weightsAll = normalizedWeight(groups, gridX, gridY, w, 2.0)
		val weightsCpAll = normalizedWeight(groups, cpX, cpY, w, 2.0)

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

			val coeff = new LUDecomposition(A).solve(B)

			val idxGrid = group.in(gridX, gridY)
			val groupedGridX = gridX(idxGrid)
			val groupedGridY = gridY(idxGrid)

			val weightsGrid = Vector(idxGrid.map(pointIndex => weightsAll(pointIndex)(groupIdx)))

			val (gridXi, gridXj) = Matrix.meshgrid(groupedGridX, groupedCpX)
			val (gridYi, gridYj) = Matrix.meshgrid(groupedGridY, groupedCpY)

			val lll = sum(kernel(norm(gridXi - gridXj, gridYi - gridYj)) ** coeff.asCol) + groupedCpZ.mean * weightsGrid

			values(idxGrid) = values(idxGrid) + weightsGrid ** lll

		}
		values
	}

	def RBF(gridX: Vector, gridY: Vector, cpX: Vector, cpY: Vector, cpZ: Vector): Vector = {
		val (xi, xj) = Matrix.meshgrid(cpX, cpX)
		val (yi, yj) = Matrix.meshgrid(cpY, cpY)

		val coeff = new LUDecomposition(kernel(norm((xi - xj), (yi - yj)))).solve(cpZ asCol)

		val (gridXi, gridXj) = Matrix.meshgrid(gridX, cpX)
		val (gridYi, gridYj) = Matrix.meshgrid(gridY, cpY)

		sum(kernel(norm(gridXi - gridXj, gridYi - gridYj)) ** coeff.asCol)
	}

	def linearTrend(x: Vector, y: Vector, z: Vector): (Double, Double, Double) = {
		val c1 = 1.0
		val c2 = x - x.mean
		val c3 = y - y.mean

		val A = Matrix(
			Row(    c1 * c1 , sum(c2 *  c1), sum(c3 *  c1) ),
			Row(sum(c1 * c2), sum(c2 ** c2), sum(c3 ** c2) ),
			Row(sum(c1 * c3), sum(c2 ** c3), sum(c3 ** c3) ))

		val B = Col(sum(c1 *  z), sum(c2 ** z), sum(c3 ** z))

		val coeff = new CholeskyDecomposition(A).solve(B)
		(coeff(0), coeff(1), coeff(2))
	}

	def l(c: (Double, Double, Double), x: Vector, y: Vector): Vector = c._1 + c._2 * (x - x.mean) + c._3 * (y - y.mean)
}