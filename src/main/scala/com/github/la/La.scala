package com.github.la

import collection._
import mutable.Builder
import generic._
import org.apache.commons.math3.util.FastMath

package object La {

	implicit def double2scalarProxy(d: Double): ScalarProxy = new ScalarProxy(d)
	implicit def scalarProxy2double(sp: ScalarProxy): ScalarProxy = sp.self

	//implicit def vector2vectorProxy(v: Vector[Double]): VectorProxy = new VectorProxy(v.map(new ScalarProxy(_)))
	//implicit def vector2vectorScalar(v: Vector[Double]): Vector[ScalarProxy] = v.map(new ScalarProxy(_))
	
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

	def sqrt[Ind, Repr <: VectorLike[Ind, Repr]](seq: VectorLike[Ind, Repr]): VectorLike[Ind, Repr] = 
		seq.map(s => FastMath.sqrt(s))

	def sin[Ind, Repr <: VectorLike[Ind, Repr]](seq: VectorLike[Ind, Repr]): VectorLike[Ind, Repr] = 
		seq.map(s => FastMath.sin(s))

	def cos[Ind, Repr <: VectorLike[Ind, Repr]](seq: VectorLike[Ind, Repr]): VectorLike[Ind, Repr] = 
		seq.map(s => FastMath.cos(s))	
}