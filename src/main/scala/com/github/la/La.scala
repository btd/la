package com.github.la

import collection._
import mutable.Builder
import generic._
import org.apache.commons.math3.util.FastMath

package object La {

	implicit def double2scalarProxy(d: Double): ScalarProxy = new ScalarProxy(d)
	implicit def scalarProxy2double(sp: ScalarProxy): Double = sp.self

	implicit def numericRange2Vactor(nr: collection.immutable.NumericRange[Double]): Vector = Vector(nr :_*)

	implicit def vector2col(v: Vector) = v.asCol
	
	implicit object ScalarProxyFactory extends VectorLikeFactory[Int, ScalarProxy] {
		def make(values: Seq[Double], size: Int): ScalarProxy = {
			require(values.size == 1 && size == 1)
			new ScalarProxy(values(0))
		}
	}

	implicit object VectorFactory extends VectorLikeFactory[Int, Vector] {
		def make(values: Seq[Double], size: Int): Vector = {
			require(values.size == size)
			Vector(values: _*)
		}
	}

	implicit object MatrixFactory extends VectorLikeFactory[(Int, Int), Matrix] {
		def make(values: Seq[Double], size: (Int, Int)): Matrix = {
			require(values.size == size._1 * size._2)
			Matrix(values.toArray, size._1, size._2)
		}
	}

	def sqrt[Ind, Repr <: VectorLike[Ind, Repr]](seq: VectorLike[Ind, Repr]): Repr = 
		seq.map(s => FastMath.sqrt(s))

	def sin[Ind, Repr <: VectorLike[Ind, Repr]](seq: VectorLike[Ind, Repr]): Repr = 
		seq.map(s => FastMath.sin(s))

	def cos[Ind, Repr <: VectorLike[Ind, Repr]](seq: VectorLike[Ind, Repr]): Repr = 
		seq.map(s => FastMath.cos(s))

	def exp[Ind, Repr <: VectorLike[Ind, Repr]](seq: VectorLike[Ind, Repr]): Repr = 
		seq.map(s => FastMath.exp(s))

	def abs[Ind, Repr <: VectorLike[Ind, Repr]](seq: VectorLike[Ind, Repr]): Repr = 
		seq.map(s => FastMath.abs(s))

	def ln[Ind, Repr <: VectorLike[Ind, Repr]](seq: VectorLike[Ind, Repr]): Repr = 
		seq.map(s => if(s < 0.0) 0.0 else FastMath.log(s))

	val Pi = math.Pi

	def sum(m: Matrix):Vector = {
		m.byRowsIndex.map(m(_, all)).foldLeft(Vector.zeros(m.numCols))((v, i) => v + i)
	}

	def sum(m: Vector):Double = m.self.sum
	
	def hann[IndexType, Repr <: VectorLike[IndexType, Repr]](x: VectorLike[IndexType, Repr]): Repr = 
		(0.5 - 0.5 * cos(2 * Pi * x))

	def pow[Ind, Repr <: VectorLike[Ind, Repr]](seq: VectorLike[Ind, Repr], p: Double): Repr = 
		seq.map(s => FastMath.pow(s, p))

	def stddev(v1: Vector, v2:Vector) = {
		val t = v1 - v2
		sqrt(sum(t ** t) / t.size)
	}

	def dist(v1: Vector, v2: Vector) = sqrt(sum(pow(v1 - v2, 2.0)))

	def dist(p1: (Double, Double), p2: (Double, Double)): Double = {
		val v1 = Vector(p1._1, p1._2)
		val v2 = Vector(p2._1, p2._2)

		dist(v1, v2)
	}

	def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
		val p = new java.io.PrintWriter(f)
		try { op(p) } finally { p.close() }
	}
}