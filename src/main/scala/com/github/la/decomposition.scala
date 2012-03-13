package com.github.la

import org.apache.commons.math3.util.FastMath
import La._

class CholeskyDecomposition(a: Matrix) {
	private val DEFAULT_ABSOLUTE_POSITIVITY_THRESHOLD = 1.0e-10
    require(a.square_?, "Matrix must be square")
    require(a.symmetric_?, "Matrix must be symmetric")
    require(!a.empty_?, "Matrix must not be empty")

	
    val n = a.numRows
    val L = Matrix.zeros( n, n )
    for (i <- 0 until n) {
        L((i, i)) = sqrt( a(i, i) - sum(pow(L(i, all), 2.0)))

        for (j <- (i + 1) until n) {
            L((j, i)) = ( a(j, i) - sum(L(i, all) ** L(j, all)) ) / L(i, i)
        }
    }
    printToFile(new java.io.File("ltData")) { f =>
        f.write(L.toString)
        
        f.write(a.toString)
    }
    private val lTData = Array.tabulate(n, n)(L(_, _))
    /*for (idx <- 0 until a.numRows){

        // check off-diagonal elements (and reset them to 0)
        for (j <- idx + 1 until a.numRows) {
            lTData(j)(idx) = 0
       }
    }

    printToFile(new java.io.File("ltData")) { f =>
    	for { 
    		idx <- 0 until a.numRows
    		ltI = lTData(idx)
    	} {
    		//require(ltI(idx) > DEFAULT_ABSOLUTE_POSITIVITY_THRESHOLD, "Matrix not positive defined")
            f.write(ltI(idx).toString + "-> ")
    		ltI(idx) = FastMath.sqrt(ltI(idx))
            f.write(ltI(idx).toString + " (")

    		val inverse = 1.0 / ltI(idx)
            f.write(inverse + ")\n")

    		for (q <- ltI.length-1 until idx by -1) {
                ltI(q) *= inverse

                val ltQ = lTData(q)

                for (p <- q until ltQ.size) {
                    ltQ(p) -= ltI(q) * ltI(p)                
                }
            }
            lTData.foreach { a1 =>
                a1.foreach { a2 =>
                    f.write(a2.toString + " ")
                }
                f.write("\n")
            }
    	}
    }



	val L = Matrix(Array.concat(lTData: _*), lTData.length, lTData(0).length)
*/
	def solve(b: Col): Row = {
		require(b.size == L.numRows, "Right side vector must be the same size = number of matrix rows")

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