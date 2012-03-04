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

	def +(other: Vector): Vector = other + self
	def -(other: Vector): Vector = other - self
	def /(other: Vector): Vector = other.map(self / _)
	def *(other: Vector): Vector = other * self

	def +(other: Matrix): Matrix = other + self
	def -(other: Matrix): Matrix = other - self
	def /(other: Matrix): Matrix = other.map(self / _)
	def *(other: Matrix): Matrix = other * self


	override def toString = self.toString

}




class Vector private[la] (val self: Array[Double]) extends VectorLike[Int, Vector] {
	val size: Int = self.size

	def indexes = 0 until size

	@inline def map(f: Double => Double): Vector = Vector(self.map(f))

	def apply(idx: Int): Double = self(idx)

	//def apply(idx: Int): ScalarProxy = self(idx)
	def apply(idx: Range): Vector = Vector(idx.map(apply(_)): _*)

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
}

case class Col(arr: Array[Double]) extends Vector(arr)
case class Row(arr: Array[Double]) extends Vector(arr)

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

	def apply(row: Int, col: Int): Double = self(row * numRows + col)

	def apply(rows: Range, col: Int): Col = Vector(rows.map(row => apply(row, col)): _*).asCol

	def apply(row: Int, cols: Range): Row = Vector(cols.map(col => apply(row, col)): _*).asRow

	def apply(rows: Range, cols: Range): Matrix = 
		Matrix(rows.map(row => this(row, cols)): _*)

	//for all
	def apply(rows: all, col: Int): Col = apply(byRowsIndex, col)

	def apply(rows: all, cols: Range): Matrix = apply(byRowsIndex, cols)

	def apply(row: Int, cols: all): Row = {
		val begin = row * numCols
		Row(self.slice(begin, begin + numCols))
	}

	def apply(rows: Range, cols: all): Matrix = Matrix((rows.map(r => apply(r, all)): _*))

	override def toString = {
		0 until numRows map (this(_, all)) mkString ("[", ";\n", "]")
	}

	def square_? = numRows == numCols
	def symmetric_? = {
		square_? && !{
			0 until numRows flatMap {r => 0 until r map {c => (r, c)}	} exists { p => this(p) != this(p.swap) }
		}
	}

	def empty_? = numRows <= 0 || numCols <= 0
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
}
