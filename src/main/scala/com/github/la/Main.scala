package com.github.la

object Main extends App {
   import La._
   
   def func(x: Vector*): Vector = sin((x(0) + x(1)) * 10) + cos((x(0) - x(1)) * 5)

   val numberOfContolPoints = 400
   val dim = 2

   val cpX = Vector.rand(numberOfContolPoints)
   val cpY = Vector.rand(numberOfContolPoints)
   val cpZ = func(cpX, cpY)

   
   val area = SquareArea(0.0, 0.0, 1.0, 1.0)
   val groups = area.iterate(CircularArea(0.0, 0.0, 0.3), 0.3, 0.3)

   val gridStep = 0.05

   val v = Vector(0.0 to 1.0 by gridStep)

   val gridX = v.asRow.repeat(v.size).asVector
   val gridY = v.asCol.repeat(v.size).asVector
   val gridZ = func(gridX, gridY)

   def stddev(v1: Vector, v2:Vector) = {
      val t = v1 - v2
      sqrt(sum(t ** t) / t.size)
   }

   val z_byPU = Approximation.PU(gridX, gridY, cpX, cpY, cpZ, groups)
   val z_byPUquad = Approximation.PUquad(gridX, gridY, cpX, cpY, cpZ, groups)
   val z_byRBF = Approximation.RBF(gridX, gridY, cpX, cpY, cpZ)
   

   println("Error of PU: " + stddev(gridZ, z_byPU))
   println("Error of PUquad: " + stddev(gridZ, z_byPUquad))
   println("Error of RBF: " + stddev(gridZ, z_byRBF))
}