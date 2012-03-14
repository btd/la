package com.github.la

import org.apache.commons.math3.util.FastMath
import La._

class CholeskyDecomposition(a: Matrix) {
    require(a.square_?, "Matrix must be square")
    require(a.symmetric_?, "Matrix must be symmetric")
    require(!a.empty_?, "Matrix must not be empty")
    //TODO check positive definess
	
    private val n = a.numRows

    val L = Matrix.zeros( n, n )

    for (i <- 0 until n) {
        L((i, i)) = sqrt( a(i, i) - sum(pow(L(i, all), 2.0)))

        for (j <- (i + 1) until n) {
            L((j, i)) = ( a(j, i) - sum(L(i, all) ** L(j, all)) ) / L(i, i)
        }
    }
 
	def solve(b: Col): Row = {
		require(b.size == n, "Right side vector must be the same size = number of matrix rows")

        val x = Vector.zeros(n)

        // Solve LY = b
        for (j <- 0 until n) {
            x(j) = b(j) / L(j, j)
        	for{i <- j + 1 until n} {
        		x(i) = b(i) - x(j) * L(j, i)
        	}
        }

        // Solve LTX = Y
        for (j <- n - 1 to 0 by -1) {
            x(j) /= L(j, j)
            for (i <- 0 until j) {
                x(i) -= x(j) * L(i,j)
            }
        }
        x asRow
	}
}

class LUDecomposition(a: Matrix) {
    /** Default bound to determine effective singularity in LU decomposition. */
    private val DEFAULT_TOO_SMALL = 1e-11

    require(a.square_?, "Matrix must be square")
    require(!a.empty_?, "Matrix must not be empty")

    private val m = a.numRows

    private val lu: Array[Array[Double]] = Array.tabulate(m,m)(a(_,_))

    private val pivot: Array[Int] = Array.tabulate(m)(i => i)

    private var even     = true

    // Loop over columns
    for (col <- 0 until m) {

        // upper
        for (row <- 0 until col) {
            val luRow = lu(row)
            var sum = luRow(col)
            for (i <- 0 until row) {
                sum -= luRow(i) * lu(i)(col)
            }
            luRow(col) = sum
        }

        // lower
        var max = col // permutation row
        var largest = Double.NegativeInfinity
        for (row <- col until m) {
            val luRow = lu(row)
            var sum = luRow(col)
            for (i <- 0 until col) {
                sum -= luRow(i) * lu(i)(col)
            }
            luRow(col) = sum

            // maintain best permutation choice
            if (FastMath.abs(sum) > largest) {
                largest = FastMath.abs(sum)
                max = row
            }
        }

        // Singularity check
        if (FastMath.abs(lu(max)(col)) < DEFAULT_TOO_SMALL) {
            throw new SingularMatrixException
        }

        // Pivot if necessary
        if (max != col) {
            var tmp = 0.0
            val luMax = lu(max)
            val luCol = lu(col)
            for (i <- 0 until m) {
                tmp = luMax(i)
                luMax(i) = luCol(i)
                luCol(i) = tmp
            }
            val temp = pivot(max)
            pivot(max) = pivot(col)
            pivot(col) = temp
            even = !even
        }

        // Divide the lower elements by the "winning" diagonal elt.
        val luDiag = lu(col)(col)
        for (row <- col + 1 until m) {
            lu(row)(col) /= luDiag
        }
    }

    def solve(b: Col): Row = {
        require(b.size == m, "Right side vector must be the same size = number of matrix rows")

        val bp: Array[Double] = Array.tabulate(m)(row => b(pivot(row)))

        // Solve LY = b
        for (col <- 0 until m) {
            val bpCol = bp(col)
            for (i <- col + 1 until m) {
                bp(i) -= bpCol * lu(i)(col)
            }
        }

        // Solve UX = Y
        for (col <- m - 1 to 0 by -1) {
            bp(col) /= lu(col)(col)
            val bpCol = bp(col)
            for (i <- 0 until col) {
                bp(i) -= bpCol * lu(i)(col)
            }
        }

        Vector(bp) asRow
    }
}

class SingularMatrixException extends Exception {

}