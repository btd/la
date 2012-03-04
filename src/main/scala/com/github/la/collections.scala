package com.github.la

import math.Numeric._
import La._

class ScalarProxy(val self: Double) extends VectorLike[Int, ScalarProxy] {

	def size = 1
	@inline def map(f: Double => Double): ScalarProxy = new ScalarProxy(f(self))

	def indexes = Seq(0)

	def apply(idx: Int): Double = {
		require(idx == 0)
		self
	}

	def +[Repr <: VectorLike[_, Repr]](other: VectorLike[_, Repr]): Repr = 
		other + self

	def -[Repr <: VectorLike[_, Repr]](other: VectorLike[_, Repr]): Repr =
		other - self

	def *[Repr <: VectorLike[_, Repr]](other: VectorLike[_, Repr]): Repr = 
		other * self

	def /[Repr <: VectorLike[_, Repr]](other: VectorLike[_, Repr]): Repr = 
		other.map(self / _)

	override def toString = self.toString

}




class Vector private[la] (val self: Array[Double]) extends VectorLike[Int, Vector] {
	val size: Int = self.size

	def indexes = 0 until size

	@inline def map(f: Double => Double): Vector = Vector(self.map(f))

	def apply(idx: Int): Double = self(idx)

	def update(idx: Int, v: Double) { self(idx) = v }

	def apply(idx: Seq[Int]): Vector = Vector(idx.map(apply(_)): _*)

	def update(idx: Seq[Int], v: Vector) {
		for{
			(i, j) <- idx.zip(indexes)
			value = v(j)
		} update(i, value)
	}

	def apply(idx: all): Vector = this(indexes)

	override def equals (that: Any): Boolean = {
		if(that.isInstanceOf[Vector]) {
			val other = that.asInstanceOf[Vector]
			size == other.size && !self.zip(other.self).exists(p => p._1 != p._2)
		} else false
	}

	override def toString = self.mkString("[", ",", "]")

	def asRow = Row(self)
	def asCol = Col(self)
}

object Vector {
	def apply(arr: Double*) = new Vector(arr.toArray)
	def apply(arr: Array[Double]) = new Vector(arr)
	def apply(arr: collection.immutable.NumericRange[Double]) = new Vector(arr.toArray)

	def rand(count: Int) = fill(count)(util.Random.nextDouble)

	def zeros(count: Int) = fill(count)(0.0)

	def fill(count: Int)(f: => Double) = {
		require(count > 0)
		apply(Array.fill(count)(f))
	}
}

case class Col(arr: Array[Double]) extends Vector(arr) {
	def repeat(n: Int): Matrix = {
		require(n > 0)

		Matrix.tabulate(arr.length, n)((i, j) => arr(i))
	}
}
case class Row(arr: Array[Double]) extends Vector(arr) {
	def repeat(n: Int): Matrix = {
		require(n > 0)

		Matrix.tabulate(n, arr.length)((i, j) => arr(j))
	}
}

object Col {
	def apply(arr: Double*):Col = Col(arr.toArray)
}

object Row {
	def apply(arr: Double*):Row = Row(arr.toArray)
}

class Matrix private[la] (val self: Array[Double], val numRows: Int, val numCols: Int)
		extends VectorLike[(Int, Int), Matrix] {	

	def apply(idx: (Int, Int)): Double = this(idx._1, idx._2)

	val size: (Int, Int) = (numRows, numCols)

	def indexes = collection.immutable.Vector.tabulate(numRows, numCols)((i, j) => i -> j).flatten

	def byRowsIndex = 0 until numRows

	@inline def map(f: Double => Double): Matrix = new Matrix(self.map(f), numRows, numCols)

	def apply(row: Int, col: Int): Double = self(row * numCols + col)

	def apply(rows: Seq[Int], col: Int): Col = Vector(rows.map(row => apply(row, col)): _*).asCol

	def apply(row: Int, cols: Seq[Int]): Row = Vector(cols.map(col => apply(row, col)): _*).asRow

	def apply(rows: Seq[Int], cols: Seq[Int]): Matrix = 
		Matrix(rows.map(row => this(row, cols)): _*)

	//for all
	def apply(rows: all, col: Int): Col = apply(byRowsIndex, col)

	def apply(rows: all, cols: Seq[Int]): Matrix = apply(byRowsIndex, cols)

	def apply(row: Int, cols: all): Row = {
		val begin = row * numCols
		Row(self.slice(begin, begin + numCols))
	}

	def apply(rows: Seq[Int], cols: all): Matrix = Matrix((rows.map(r => apply(r, all)): _*))

	override def toString = {
		byRowsIndex map (this(_, all)) mkString ("[", ";\n", "]")
	}

	def square_? = numRows == numCols
	def symmetric_? = {
		square_? && !{
			byRowsIndex flatMap {r => 0 until r map {c => (r, c)}	} exists { p => this(p) != this(p.swap) }
		}
	}

	def empty_? = numRows <= 0 || numCols <= 0

	def asVector = Vector(self)

	def *(r: Row): Matrix = this ** r.repeat(numRows)

	def *(c: Col): Matrix = this ** c.repeat(numCols)
}

object Matrix {
	def rand(rows: Int, cols: Int) = {
		require(rows > 0 && cols > 0)
		new Matrix(Array.fill(rows * cols)(util.Random.nextDouble), rows, cols)
	}

//	def apply(value: Seq[Seq[Double]]): Matrix = apply(value.flatten.toArray, value.size, value.headOption.map(_.size).getOrElse(0))

	def apply(arr: Array[Double], rows: Int, cols: Int): Matrix = new Matrix(arr, rows, cols)

	
	def apply(rows: Row*): Matrix = apply(Array.concat((rows.map(_.arr): _*)), rows.size, rows.headOption.map(_.size).getOrElse(0))

	def tabulate(rows: Int, cols: Int)(f: (Int, Int) => Double) = {
		val v = collection.immutable.Vector.tabulate(rows, cols)(f).map(Row(_:_*))//жесть

		Matrix(v:_*)
	}

	def meshgrid(x: Vector, y: Vector): (Matrix, Matrix) = {
		(x.asRow.repeat(y.size), y.asCol.repeat(x.size))
	}
}
