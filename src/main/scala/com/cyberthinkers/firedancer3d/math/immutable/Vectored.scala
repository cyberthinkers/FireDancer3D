package com.cyberthinkers.firedancer3d.math.immutable

trait Vectored {
   def vectored: Vector[Double]
   def isValid = if(vectored.find(d => d == Double.NaN || d == Double.PositiveInfinity || d == Double.NegativeInfinity) == Some()) false else true
}