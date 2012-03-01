package com.github.la

import org.specs2.mutable._
import La._

class LaSpec extends Specification {

	val eps = 10E-7
	def compareDoubles(a: Double, b: Double): Boolean = math.abs(a-b) <= eps

	"Vector" should {

	  "can add other vector" in {
	    Vector(1.0, 2.0) + Vector(3.0, 10.0) ==== Vector(4.0, 12.0)
	  }

	  "can substract other vector" in {
	    Vector(1.0, 2.0) - Vector(3.0, 10.0) ==== Vector(-2.0, -8.0)
	  }

	  "can be multiply by elements" in {
	    Vector(1.0, 2.0) ** Vector(3.0, 10.0) ==== Vector(3.0, 20.0)
	  }

	}

	"Library" should {

		"calc sin of vector" in {
			sin(Vector(0.0)) ==== Vector(0.0)
		}
		"calc cos of vector" in {
			cos(Vector(0.0)) ==== Vector(1.0)
		}
		"calc sin of scalar" in {
			sin(0.0) ==== 0.0
		}
	}
}