package com.github.la

import collection._
import mutable.Builder
import generic._
import org.apache.commons.math3.util.FastMath

package object La {

	implicit def double2scalarProxy(d: Double): ScalarProxy = new ScalarProxy(d)
	implicit def scalarProxy2double(sp: ScalarProxy): ScalarProxy = sp.self

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

	val Pi = math.Pi

	def sum(m: Matrix):Vector = {
		m.byRowsIndex.map(m(_, all)).foldLeft(Vector.zeros(m.numCols))((v, i) => v + i)
	}

	def sum(m: Vector):Double = m.self.sum
	
}