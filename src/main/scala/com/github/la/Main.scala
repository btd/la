package com.github.la

object Main extends App {
   import La._
   import Vector._
   
   def func(x: Vector*): Vector = sin((x(0) + x(1)) * 10) + cos((x(0) - x(1)) * 5)

   val numberOfContolPoints = 400
   val dim = 2

   val mt = .01
   val vv = 0.0 to 1.0 by .05

   val cpX = Vector.concat(rand(numberOfContolPoints), zeros(vv.size) + mt, vv, ones(vv.size) - mt, vv)
   val cpY = Vector.concat(rand(numberOfContolPoints), vv, zeros(vv.size) + mt, vv, ones(vv.size) - mt)
   val cpZ = func(cpX, cpY)

   
   val area = SquareArea(0.0, 0.0, 1.0, 1.0)
   val groups = area.iterate(CircularArea(0.0, 0.0, 0.3), 0.3, 0.3)

   val gridStep = Pi / 100

   val v = 0.0 to 1.0 by gridStep

   val gridX = v.asRow.repeat(v.size).asVector
   val gridY = v.asCol.repeat(v.size).asVector
   val gridZ = func(gridX, gridY)

   

   var z_byPU = Approximation.PU(gridX, gridY, cpX, cpY, cpZ, groups)
   var z_byPUquad = Approximation.PUquad(gridX, gridY, cpX, cpY, cpZ, groups)
   var z_byRBF = Approximation.RBF(gridX, gridY, cpX, cpY, cpZ)

   println("Error of PU: " + stddev(gridZ, z_byPU))
   println("Error of PUquad: " + stddev(gridZ, z_byPUquad))
   println("Error of RBF: " + stddev(gridZ, z_byRBF))


   val splittedAreas = area.split(cpX, cpY, 7)

   z_byPU = Approximation.PU(gridX, gridY, cpX, cpY, cpZ, splittedAreas)
   z_byPUquad = Approximation.PUquad(gridX, gridY, cpX, cpY, cpZ, splittedAreas)

   //splittedAreas.foreach{ a =>
   //   println(a.toString + a.in(gridX, gridY).toString)
   //}
   //for {idx <- z_byPU.indexes} {
   //   println("Must be : f(%.3f, %.3f) = %.3f but %.3f".format(gridX(idx), gridY(idx), gridZ(idx), z_byPU(idx)))
   //}

   println("Error of PU: " + stddev(gridZ, z_byPU))
   println("Error of PUquad: " + stddev(gridZ, z_byPUquad))
   println("Error of RBF: " + stddev(gridZ, z_byRBF))
}