package org.firedancer3d.math

import Math._

case class Vect2(x:Double, y:Double) extends Vectored {

  def this(v: Vect2) = this(v.x, v.y)

  def this(t: (Double, Double)) = this(t._1, t._2)

  def vectored: Vector[Double] = Vector(x, y)
  
  def +(that: Vect2) = Vect2(this.x + that.x, this.y + that.y)
  def -(that: Vect2) = Vect2(this.x - that.x, this.y - that.y)
  def *(that: Vect2) = Vect2(this.x * that.x, this.y * that.y)
  def /(that: Vect2) = Vect2(this.x / that.x, this.y / that.y)
  
  def *(d: Double) = Vect2(x * d, y * d)
  def /(d: Double) = Vect2(x / d, y / d)
  
  def unary_- = Vect2(-x, -y)
  
  def ==~(that: Vect2): Boolean = {
    val epsilon = (1e-4)
    abs(this.x - that.x) < epsilon &&
    abs(this.y - that.y) < epsilon 
  }
  
  def length = Math.sqrt(lengthSquared)
  
  def lengthSquared = x * x + y * y
  
  def normalize = {
    val d = length
    if(d > 0.0) {
      val f = 1.0 / d
      new Vect2(f * x, f * y)
    } else {
      this
    }      
  }
  
  def lerp(that: Vect2, d: Double) = {
    val m = 1 - d
    Vect2(m * this.x + d * that.x, m * this.y + d * that.y)
  }
}

object Vect2 {
  val zero = Vect2
  val one = Vect2(1, 1)
  val negativeOne = Vect2(-1, -1)
  val unitX = Vect2(1, 0)
  val unitY = Vect2(0, 1)
  val negativeUnitX = Vect2(-1, 0)
  val negativeUnitY = Vect2(0, -1)
} 