package org.firedancer3d.math

trait Vectored {
   val epsilon = (1e-4)
   def vectored: Vector[Double] 
   def isValid: Boolean = if(vectored.find(d => d.isNaN() || d.isInfinity) == None) true else false
}