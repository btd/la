package com.github.la

import org.apache.commons.math3.util.FastMath
import La._

class CholeskyDecomposition(a: Matrix) {
	private val DEFAULT_ABSOLUTE_POSITIVITY_THRESHOLD = 1.0e-10
    require(a.square_?, "Matrix must be square")
    require(a.symmetric_?, "Matrix must be symmetric")
    require(!a.empty_?, "Matrix must not be empty")

	private val lTData = Array.tabulate(a.numRows, a.numCols)(a(_, _))

    for (idx <- 0 until a.numRows){

        // check off-diagonal elements (and reset them to 0)
        for (j <- idx + 1 until a.numRows) {
            lTData(j)(idx) = 0
       }
    }

	for { 
		idx <- 0 until a.numRows
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
	}

	val L = Matrix(Array.concat(lTData: _*), lTData.length, lTData(0).length)

	def solve(b: Col): Row = {
		require(b.size == L.numRows)

		val x = b.arr 

        // Solve LY = b
        for (j <- 0 until x.length) {
        	val lJ = lTData(j)
        	x(j) /= lJ(j)
        	val xJ = x(j)
        	for{i <- j + 1 until x.length} {
        		x(i) -= xJ * lJ(i)
        	}
        }

        // Solve LTX = Y
        for (j <- x.length - 1 to (0, -1)) {
            x(j) /= lTData(j)(j)
            val xJ = x(j)
            for (i <- 0 until j) {
                x(i) -= xJ * lTData(i)(j)
            }
        }
        Vector(x).asRow
	}
}