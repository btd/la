package com.github.la

import org.apache.commons.math3.util.FastMath

class CholeskyDecomposition(m: Matrix) {
	private val DEFAULT_ABSOLUTE_POSITIVITY_THRESHOLD = 1.0e-10

	require(m.square_? && m.symmetric_?)

	private val lTData = Array.tabulate(m.numRows, m.numCols)(m(_, _).self)

	private val builder = new collection.immutable.VectorBuilder[Vector[Double]]()

	for { 
		idx <- 0 until m.numRows
		ltI = lTData(idx)
	} {
		require(ltI(idx) > DEFAULT_ABSOLUTE_POSITIVITY_THRESHOLD)
		ltI(idx) = FastMath.sqrt(ltI(idx))

		val inverse = 1.0 / ltI(idx)

		for (q <- ltI.length-1 until (idx, -1)) {
            ltI(q) *= inverse

            val ltQ = lTData(q)

            for (p <- q until ltQ.size) {
                ltQ(p) -= ltI(q) * ltI(p)                
            }
        }
        builder += Vector((ltI: _*))
	}

	val L = Matrix(builder.result)

	def solve(b: VectorLike[Int, _]): VectorLike[Int, _]= {
		require(b.size == L.numRows)

		val x = Array.tabulate(b.size)(b(_).self)

        // Solve LY = b
        for (j <- 0 until m) {
        	val lJ = lTData(j)
        	x(j) /= lJ(j)
        	val xJ = x(j)
        	for{i <- j + 1 until m} {
        		x(i) -= xJ * lJ(i)
        	}
        }

        // Solve LTX = Y
        for (j <- m - 1 to (0, -1)) {
            x(j) /= lTData(j)(j)
            val xJ = x(j)
            for (i <- 0 until j) {
                x(i) -= xJ * lTData(i)(j)
            }
        }
        Vector[Double]((x: _*))
	}

}