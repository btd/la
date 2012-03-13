package com.github.la

object Main extends App {
   import La._
   import Vector._

   
   
   def func(x: Vector*): Vector = sin((x(0) + x(1)) * 10) + cos((x(0) - x(1)) * 5)

   val numberOfContolPoints = 10
   val dim = 2

   val mt = .0001
   val vv = 0.0 to 1.0 by .07

   //val cpX = Vector.concat(rand(numberOfContolPoints), zeros(vv.size) + mt, vv, ones(vv.size) - mt, vv)
   //val cpY = Vector.concat(rand(numberOfContolPoints), vv, zeros(vv.size) + mt, vv, ones(vv.size) - mt)
   val cpX = rand(numberOfContolPoints)
   val cpY = rand(numberOfContolPoints)
   val cpZ = func(cpX, cpY)

   
   val area = SquareArea(0.0, 0.0, 1.0, 1.0)
   val groups = area.iterate(CircularArea(0.0, 0.0, 0.2), 0.2, 0.2)

   val gridStep = 0.05

   val v = 0.0 to 1.0 by gridStep

   val gridX = v.asRow.repeat(v.size).asVector
   val gridY = v.asCol.repeat(v.size).asVector
   val gridZ = func(gridX, gridY)

  

   //var z_byPU = Approximation.PU(gridX, gridY, cpX, cpY, cpZ, groups)
   //var z_byPUquad = Approximation.PUquad(gridX, gridY, cpX, cpY, cpZ, groups)
   var z_byRBF = Approximation.RBF(gridX, gridY, cpX, cpY, cpZ)

   println("Error of RBF: " + stddev(gridZ, z_byRBF))

   //println(">>> Quality with simple circular iteration:")
   //println("Error of PU: " + stddev(gridZ, z_byPU))
   //println("Error of PUquad: " + stddev(gridZ, z_byPUquad))


   //printToFile(new java.io.File("test")) { f =>
   //   f.write(z_byPU.toString)
   //}
/*
   val points: Seq[(Double, Double)] = cpX.indexes.map(idx => (cpX(idx), cpY(idx)))

   val takeNumber = 20
   val rTh = 0.00001
   val groups2: Seq[CircularArea] = points.map { p => 
      val in = points.zipWithIndex.sortBy(p1 => dist(p, p1._1)).take(takeNumber + 1)//because of first will be p
      CircularArea(p._1, p._2, dist(p, in.last._1) + rTh) -> in.map(_._2)
   }.groupBy(p => p._2).values.map(_.head._1).toSeq

   z_byPU = Approximation.PU(gridX, gridY, cpX, cpY, cpZ, groups2)
   z_byPUquad = Approximation.PUquad(gridX, gridY, cpX, cpY, cpZ, groups2)
   //var z_byRBF = Approximation.RBF(gridX, gridY, cpX, cpY, cpZ)
   println(">>> Quality with limited circles:")
   println("Error of PU: " + stddev(gridZ, z_byPU))
   println("Error of PUquad: " + stddev(gridZ, z_byPUquad))


   z_byPU = Approximation.PU(cpX, cpY, cpX, cpY, cpZ, groups2)
   z_byPUquad = Approximation.PUquad(cpX, cpY, cpX, cpY, cpZ, groups2)
   //var z_byRBF = Approximation.RBF(gridX, gridY, cpX, cpY, cpZ)
   println(">>> Quality with limited circles:")
   println("Error of PU: " + stddev(cpZ, z_byPU))
   println("Error of PUquad: " + stddev(cpZ, z_byPUquad))

*/   
  /* val splittedAreas = area.split(cpX, cpY, 10)

   val t = 1.0
   val splittedAreas2 = splittedAreas.map { a =>
      SquareArea(a.x1 - t * a.width, a.y1 - t * a.height, a.x2 + t * a.width,a.y2 + t * a.height)
   }
   
   z_byPU = Approximation.PU(gridX, gridY, cpX, cpY, cpZ, splittedAreas2)
   z_byPUquad = Approximation.PUquad(gridX, gridY, cpX, cpY, cpZ, splittedAreas2)

   println(">>> Quality with subdivison:")

   println("Error of PU: " + stddev(gridZ, z_byPU))
   println("Error of PUquad: " + stddev(gridZ, z_byPUquad))
  

   val circ1 = splittedAreas.flatMap { a =>
      val d = math.max(a.height, a.width)
      List(
         CircularArea(a.square._1._1, a.square._1._2, d / 2.0),
         CircularArea(a.square._2._1, a.square._1._2, d / 2.0),
         CircularArea(a.square._1._1, a.square._2._2, d / 2.0),
         CircularArea(a.square._2._1, a.square._2._2, d / 2.0),
         CircularArea(a.center._1, a.center._1, d / 2.0)
      )
   }


   z_byPU = Approximation.PU(gridX, gridY, cpX, cpY, cpZ, circ1)
   z_byPUquad = Approximation.PUquad(gridX, gridY, cpX, cpY, cpZ, circ1)

   println(">>> Quality with subdivison=>circ by 1/2 :")

   println("Error of PU: " + stddev(gridZ, z_byPU))
   println("Error of PUquad: " + stddev(gridZ, z_byPUquad))
   

   val circ2 = splittedAreas.flatMap { a =>
      val r = dist(a.square._1, a.center)
      List(
         CircularArea(a.square._1._1, a.square._1._2, r),
         CircularArea(a.square._2._1, a.square._1._2, r),
         CircularArea(a.square._1._1, a.square._2._2, r),
         CircularArea(a.square._2._1, a.square._2._2, r)
      )
   }


   z_byPU = Approximation.PU(gridX, gridY, cpX, cpY, cpZ, circ2)
   z_byPUquad = Approximation.PUquad(gridX, gridY, cpX, cpY, cpZ, circ2)

   println(">>> Quality with subdivison=>circ by center:")

   println("Error of PU: " + stddev(gridZ, z_byPU))
   println("Error of PUquad: " + stddev(gridZ, z_byPUquad))
  

   val circ3 = splittedAreas.flatMap { a =>
      val d = math.max(a.height, a.width)
      List(
         CircularArea(a.square._1._1, a.square._1._2, d),
         CircularArea(a.square._2._1, a.square._1._2, d),
         CircularArea(a.square._1._1, a.square._2._2, d),
         CircularArea(a.square._2._1, a.square._2._2, d)
      )
   }


   z_byPU = Approximation.PU(gridX, gridY, cpX, cpY, cpZ, circ3)
   z_byPUquad = Approximation.PUquad(gridX, gridY, cpX, cpY, cpZ, circ3)

      printToFile(new java.io.File("1.txt")) { f =>
      f.write(z_byPU.toString)
   }

   println(">>> Quality with subdivison=>circ by max:")

   println("Error of PU: " + stddev(gridZ, z_byPU))
   println("Error of PUquad: " + stddev(gridZ, z_byPUquad))


   val circ4 = splittedAreas.map { a =>
      CircularArea(a.center._1, a.center._2, dist(a.square._1, a.center) + 0.001)
   }


   z_byPU = Approximation.PU(gridX, gridY, cpX, cpY, cpZ, circ4)
   z_byPUquad = Approximation.PUquad(gridX, gridY, cpX, cpY, cpZ, circ4)

   println(">>> Quality with subdivison=>circ in center square in circ:")

   println("Error of PU: " + stddev(gridZ, z_byPU))
   println("Error of PUquad: " + stddev(gridZ, z_byPUquad))
   
*/
}