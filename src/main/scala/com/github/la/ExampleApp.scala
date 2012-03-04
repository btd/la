package com.github.la



object ExampleApp extends App {
  import La._


  val m = Matrix(
  	Row(4.0, 1.0, 1.0), 
  	Row(1.0, 4.0, 1.0), 
  	Row(1.0, 1.0, 4.0)
  )

  val solver = new CholeskyDecomposition(m)

  println(solver.L)

  println(solver.solve(Col(1, 1, 1)))
}
