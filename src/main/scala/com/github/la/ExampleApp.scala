package com.github.la



object ExampleApp extends App {
  import La._


  val v = Vector[Double](math.Pi / 2)
  val v2 = sin(v) + Vector(1.0)

  println(v2)
}
