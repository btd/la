package com.github.la

trait Sizing[A] {
	def size: A

	def indexes: Seq[A]
}

trait VectorLikeFactory[IndexType, Repr <: VectorLike[IndexType, Repr]] {

	def make(values: Seq[Double], size: IndexType): Repr
}

trait VectorLike[IndexType, Repr <: VectorLike[IndexType, Repr]] extends Sizing[IndexType] {
	import La._

	def apply(idx: IndexType): Double

	def map (f: Double => Double): Repr

	private[la] def repr: Repr = this.asInstanceOf[Repr]

	protected[la] def elementWiseOp(other: Repr, op: (Double, Double) => Double)
			(f: VectorLikeFactory[IndexType, Repr]): Repr = {
		require(this.size == other.size, "Size not equals: first " + this.size + " second " + other.size)
		f.make(this.indexes.map(i => op(this(i), other(i))), size)
	}

	def +(other: Repr)(implicit f: VectorLikeFactory[IndexType, Repr]): Repr = 
		elementWiseOp(other, (a, b) => a+b)(f)

	def -(other: Repr)(implicit f: VectorLikeFactory[IndexType, Repr]): Repr = 
		elementWiseOp(other, (a, b) => a-b)(f)

	def **(other: Repr)(implicit f: VectorLikeFactory[IndexType, Repr]): Repr = 
		elementWiseOp(other, (a, b) => a*b)(f)

	def +(scalar: Double): Repr = map(_ + scalar)
	def -(scalar: Double): Repr = map(_ - scalar)
	def *(scalar: Double): Repr = map(_ * scalar)
	def /(scalar: Double): Repr = map(_ / scalar)

	def unary_- = this.map(s => -s)

	lazy val mean: Double = indexes.map(this(_)).foldLeft(0.0)((c, v) => c + v) / indexes.size.asInstanceOf[Double]
}


//for small optimization in indexing
trait all
object all extends all