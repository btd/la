package com.github.la

object Main extends App {
   import La._
   import Vector._

   
   
   def func(x: Vector*): Vector = sin((x(0) + x(1)) * 10) + cos((x(0) - x(1)) * 5)

printToFile(new java.io.File("viz.m")) { f =>

   def figure(idx: Int) = {
      f.write ( """
         figure(%d), clf, hold on;
      """.format(idx))
   }

   def plotCp = {
      f.write ( """
         plot(cpX, cpY, 'o');
      """)
   }

   def plotAreas(groups: Seq[Area]) = {
      groups.foreach { g =>
         g match {
            case CircularArea(centerX, centerY, radius) => f.write ( "circle([" + centerX + " "+centerY+"], "+radius+", 100, '--');\n")
            case SquareArea(x1, y1, x2, y2) => f.write ( "rectangle('Position',["+x1+" "+y1+" "+(x2 - x1)+" "+(y2 - y1)+"]);\n")
         }
      }
   }

   def subplot(v: Int, h: Int, n: Int) = {
      f.write ( """
         subplot(%d, %d, %d);
      """.format(v, h, n))
   }

   def exampleViz(idx: Int) = {
      subplot(2, 2, 1)
      f.write ( """
         surf(gridX, gridY, puRes%d);
      """.format(idx))

      subplot(2, 2, 2)
      f.write ( """
         surf(gridX, gridY, puQuadRes%d);
      """.format(idx))

      subplot(2, 2, 3)
      f.write ( """
         surf(gridX, gridY, puRes%d - gridZ);
      """.format(idx))

      subplot(2, 2, 4)
      f.write ( """
         surf(gridX, gridY, puQuadRes%d - gridZ);
      """.format(idx))
   }

   def exampleData(idx: Int, v1: Any, v2: Any, s: Int) = {
      f.write ( """
         puRes%d = reshape(%s, %d, %d);
         puQuadRes%d = reshape(%s, %d, %d);
      """.format(idx, v1, s, s, idx, v2, s, s))
   }

   def example(idx: Int, v1: Any, v2: Any, s: Int, groups: Seq[Area]) = {
      figure(idx * 2)
      plotCp

      plotAreas(groups)
      exampleData(idx, v1, v2, s)

      figure(idx * 2 + 1)
      exampleViz(idx)
      f.write("zlim([-2 2]);\n")
   }


   val numberOfContolPoints = 400
   val dim = 2

   val mt = -.0000001
   val vv = 0.0 to 1.0 by .07

   val shiftX = 0.0
   val shiftY = 0.0

   val cpX = Vector.concat(rand(numberOfContolPoints), zeros(vv.size) + mt, vv, ones(vv.size) - mt, vv) + shiftX
   val cpY = Vector.concat(rand(numberOfContolPoints), vv, zeros(vv.size) + mt, vv, ones(vv.size) - mt) + shiftY
   //val cpX = rand(numberOfContolPoints)
   //val cpY = rand(numberOfContolPoints)
   val cpZ = func(cpX, cpY)

   f.write ( """
      cpN = %s;
      cpX = %s;
      cpY = %s;
      cpZ = %s;
   """.format(numberOfContolPoints, cpX, cpY, cpZ))


   val gridStep = 0.05

   val v = 0.0 to 1.0 by gridStep

   val gridX = v.asRow.repeat(v.size).asVector + shiftX
   val gridY = v.asCol.repeat(v.size).asVector + shiftY
   val gridZ = func(gridX, gridY)

   f.write ( """
      gridX = reshape(%s, %d, %d);
      gridY = reshape(%s, %d, %d);
      gridZ = reshape(%s, %d, %d);
   """.format(gridX, v.size, v.size, gridY, v.size, v.size, gridZ, v.size, v.size))
   
   val area = SquareArea(0.0 + shiftX, 0.0 + shiftY, 1.0 + shiftX, 1.0 + shiftY)
   val groups = area.iterate(CircularArea(0.0 + shiftX, 0.0 + shiftY, 0.2), 0.2, 0.2)

   


   var z_byPU = Approximation.PU(gridX, gridY, cpX, cpY, cpZ, groups)
   var z_byPUquad = Approximation.PUquad(gridX, gridY, cpX, cpY, cpZ, groups)
   var z_byRBF = Approximation.RBF(gridX, gridY, cpX, cpY, cpZ)

   println("Error of RBF: " + stddev(gridZ, z_byRBF))


   println(">>> Quality with simple circular iteration:")
   println("Error of PU: " + stddev(gridZ, z_byPU))
   println("Error of PUquad: " + stddev(gridZ, z_byPUquad))

   example(1, z_byPU, z_byPUquad, v.size, groups)


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

   example(2, z_byPU, z_byPUquad, v.size, groups2)

   val splittedAreas = area.split(cpX, cpY, 10)

   val t = 1.0
   val splittedAreas2 = splittedAreas.map { a =>
      SquareArea(a.x1 - t * a.width, a.y1 - t * a.height, a.x2 + t * a.width,a.y2 + t * a.height)
   }

   
   
   z_byPU = Approximation.PU(gridX, gridY, cpX, cpY, cpZ, splittedAreas2)
   z_byPUquad = Approximation.PUquad(gridX, gridY, cpX, cpY, cpZ, splittedAreas2)

   println(">>> Quality with subdivison:")

   println("Error of PU: " + stddev(gridZ, z_byPU))
   println("Error of PUquad: " + stddev(gridZ, z_byPUquad))

   example(3, z_byPU, z_byPUquad, v.size, splittedAreas2)

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

   example(4, z_byPU, z_byPUquad, v.size, circ1) 

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

   example(5, z_byPU, z_byPUquad, v.size, circ2)
  

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

   example(6, z_byPU, z_byPUquad, v.size, circ3)

   val circ4 = splittedAreas.map { a =>
      CircularArea(a.center._1, a.center._2, dist(a.square._1, a.center) + 0.001)
   }


   z_byPU = Approximation.PU(gridX, gridY, cpX, cpY, cpZ, circ4)
   z_byPUquad = Approximation.PUquad(gridX, gridY, cpX, cpY, cpZ, circ4)

   println(">>> Quality with subdivison=>circ in center square in circ:")

   println("Error of PU: " + stddev(gridZ, z_byPU))
   println("Error of PUquad: " + stddev(gridZ, z_byPUquad))

   example(7, z_byPU, z_byPUquad, v.size, circ4)
   


   f.write ( """
      rbfRes = reshape(%s, %d, %d);
   """.format(z_byRBF, v.size, v.size))
   figure(100)
   subplot(1, 2, 1)
   f.write ( "surf(gridX, gridY, rbfRes);\n")

   subplot(1, 2, 2)
   f.write ( "surf(gridX, gridY, rbfRes - gridZ);\n")
}
}