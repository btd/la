package com.github.la

import math.Numeric._
import La._

class ScalarProxy(value: Double) extends Proxy.Typed[Double] with VectorLike[Int, ScalarProxy] {
	val self = value

	def size = 1
	@inline def map(f: ScalarProxy => ScalarProxy): ScalarProxy = new ScalarProxy(f(this).self)
	def indexes = Seq(0)
	def apply(idx: Int): Double = {
		require(idx == 0)
		self
	}

	def /(other: ScalarProxy): ScalarProxy = new ScalarProxy(self / other.self)
	def *(other: ScalarProxy): ScalarProxy = new ScalarProxy(self * other.self)

	def +(other: VectorProxy): VectorProxy = other + this
	def -(other: VectorProxy): VectorProxy = other - this
	def /(other: VectorProxy): VectorProxy = other / this
	def *(other: VectorProxy): VectorProxy = other * this

}




class VectorProxy(value: Vector[ScalarProxy]) extends VectorLike[Int, VectorProxy] {
	val self = value

	val size: Int = self.size
	def indexes = Vector.range(0, size)

	@inline def map(f: ScalarProxy => ScalarProxy): VectorProxy = new VectorProxy(self.map(f))

	def apply(idx: Int): Double = self(idx).self

	//def apply(idx: Int): ScalarProxy = self(idx)
	def apply(idx: Range): VectorProxy = {
		new VectorProxy(Vector.tabulate(idx.length)(i => self(idx(i))))
	}
	def apply(idx: all): VectorProxy = this(0 until self.size)

	def +(other: ScalarProxy): VectorProxy = new VectorProxy(self.map(_ + other))
	def -(other: ScalarProxy): VectorProxy = new VectorProxy(self.map(_ - other))
	def /(other: ScalarProxy): VectorProxy = new VectorProxy(self.map(_ / other))
	def *(other: ScalarProxy): VectorProxy = new VectorProxy(self.map(_ * other))

	override def toString = self.toString
}

case class Col(value: Vector[ScalarProxy]) extends VectorProxy(value)
case class Row(value: Vector[ScalarProxy]) extends VectorProxy(value)

class Matrix(val self: Vector[ScalarProxy], val numRows: Int, val numCols: Int)
		extends VectorLike[(Int, Int), Matrix] {

	def this(value: Vector[Vector[ScalarProxy]]) {
		this(value.flatten, value.size, value.headOption.map(_.size).getOrElse(0))
	}

	def this(v: Seq[Vector[ScalarProxy]]) {
		this(Vector((v: _*)))
	}

	def apply(idx: (Int, Int)): Double = this(idx._1, idx._2).self

	val size: (Int, Int) = (numRows, numCols)
	def indexes = Vector.tabulate(numRows, numCols)((i, j) => i -> j).flatten

	@inline def map(f: ScalarProxy => ScalarProxy): Matrix = new Matrix(self.map(f), numRows, numCols)

	def apply(row: Int, col: Int): ScalarProxy = self(row * numRows + col)

	def apply(rows: Range, col: Int): Col = Col(Vector.tabulate(rows.length)(i => apply(rows(i), col)))

	def apply(row: Int, cols: Range): Row = Row(Vector.tabulate(cols.length)(i => apply(row, cols(i))))

	def apply(rows: Range, cols: Range): Matrix = 
		new Matrix(Vector.tabulate(rows.length, cols.length)((i, j) => apply(rows(i), cols(j))))

	//for all
	def apply(rows: all, col: Int): Col = apply(0 until numRows, col)

	def apply(rows: all, cols: Range): Matrix = apply(0 until numRows, cols)

	def apply(row: Int, cols: all): Row = {
		val begin = row * numCols
		Row(self.slice(begin, begin + numCols))
	}

	def apply(rows: Range, cols: all): Matrix = new Matrix(rows.map(r => apply(r, all).value))

	def +(other: ScalarProxy): Matrix = new Matrix(self.map(_ + other), numRows, numCols)
	def -(other: ScalarProxy): Matrix = new Matrix(self.map(_ - other), numRows, numCols)
	def /(other: ScalarProxy): Matrix = new Matrix(self.map(_ / other), numRows, numCols)
	def *(other: ScalarProxy): Matrix = new Matrix(self.map(_ * other), numRows, numCols)
}
