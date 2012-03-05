package com.github.la

trait Area {
	//import Helper._

	def in_?(x: Double, y: Double, eps: Double = 10E-10): Boolean

	val center: (Double, Double)

	val square: ((Double, Double), (Double, Double))

   	def movedTo(x: Double, y: Double): Area 

    def in(x: Vector, y: Vector, eps: Double = 10E-10): Seq[Int] = {
    	require(x.size == y.size)
    	for{idx <- x.indexes; if in_?(x(idx), y(idx), eps) } yield { idx }
    }

    def scale[Ind, Repr <: VectorLike[Ind, Repr]](x: VectorLike[Ind, Repr], y: VectorLike[Ind, Repr]): (Repr, Repr) = 
		((x - square._1._1) / (square._2._1 - square._1._1), (y - square._1._2) / (square._2._2 - square._1._2) )
}

case class SquareArea(
	x1: Double,
	y1: Double,
	x2: Double,
	y2: Double) extends Area {

	require(x1 <= x2)
	require(y1 <= y2)

	val square = ((x1, y1), (x2, y2))

	private val w2 = (x2 - x1) / 2.0
	private val h2 = (y2 - y1) / 2.0

	val center = (x1 + w2, y1 + h2)

	def movedTo(x: Double, y: Double): Area = SquareArea(x - w2, y - h2, x + w2, y + h2)

	def in_?(x: Double, y: Double, eps: Double = 10E-10) = x1 - eps <= x && x <= x2 + eps && y1 - eps <= y && y <= y2 + eps

	def iterate(first: Area, stepX: Double, stepY: Double): Seq[Area] = {
		require(stepX > 0.0 && stepY > 0.0, "Step must be > 0")

		val firstCenter = first.center
		require(in_?(firstCenter._1, firstCenter._2))

		val countX: Int = ((x2 - firstCenter._1) / stepX).intValue + 1
		val countY: Int = ((y2 - firstCenter._2) / stepY).intValue + 1

		collection.immutable.Vector.tabulate(countX, countY){ (i, j) => 
			first.movedTo(firstCenter._1 + i.doubleValue * stepX, firstCenter._2 + j.doubleValue * stepY)
		}.flatten
	}

	def split(x: Vector, y: Vector, limit: Int): Seq[(SquareArea, (Vector, Vector))] = {
		import collection.mutable.Queue

		require(limit > 3)
		val areas = Queue((this, (x, y)))
		val areasUnsplittable = Queue[(SquareArea, (Vector, Vector))]()

		def split4(area: SquareArea): List[SquareArea] = {
			val ltPoint = area.square._1
			val rbPoint = area.square._2
			val centerPoint = area.center

			List(SquareArea(ltPoint._1, ltPoint._2, centerPoint._1, centerPoint._2),
				SquareArea(centerPoint._1, ltPoint._2, rbPoint._1, centerPoint._2),
				SquareArea(ltPoint._1, centerPoint._2, centerPoint._1, rbPoint._2),
				SquareArea(centerPoint._1, centerPoint._2, rbPoint._1, rbPoint._2))
		}
		while(!areas.isEmpty) {
			val top = areas.dequeue
			//println("Begin split " + top._1 + " ends " + areas.size)

			val splittedTop = split4(top._1)
			val pointsInSplitted = splittedTop.map(_.in(top._2._1, top._2._2, 10E-5))
			if(!pointsInSplitted.exists(_.size < limit)) {
				println(top._1)
				println(pointsInSplitted)
				areas.enqueue(splittedTop.zip(pointsInSplitted).map { p =>
					(p._1, (top._2._1(p._2), top._2._2(p._2)))
				}: _*)
			} else {
				//println("--")
				areasUnsplittable.enqueue(top)
			}
		}
		areasUnsplittable
	}
}

case class CircularArea(cX: Double, cY: Double, r: Double) extends Area {
   require(r >= 0.0, "Radius must be >= 0")
   import math._

   val square = ((cX - r, cY - r), (cX + r, cY + r))
   val center = (cX, cY)

   def movedTo(x: Double, y: Double): Area = CircularArea(x, y, r)

   def in_?(x: Double, y: Double, eps: Double = 10E-10) = abs(cX - x) - r <= eps && abs(cY - y) -r <= eps 
}
