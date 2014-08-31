package com.cyberthinkers.firedancer3d.math

import Math._
import comimport com.cyberthinkers.firedancer3d.math.Vectored
.cyberthinkers.firedancer3d.math.immutable.Vectored
import scala.Vector

case class Vector2(x:Double, y:Double) extends Vectored {

  def this(v: Vector2) = this(v.x, v.y)

  def this(t: (Double, Double)) = this(t._1, t._2)

  def vectored: Vector[Double] = Vector(x, y)
  
  def +(that: Vector2) = Vector2(this.x + that.x, this.y + that.y)
  def -(that: Vector2) = Vector2(this.x - that.x, this.y - that.y)
  def *(that: Vector2) = Vector2(this.x * that.x, this.y * that.y)
  def /(that: Vector2) = Vector2(this.x / that.x, this.y / that.y)
  
  def *(d: Double) = Vector2(x * d, y * d)
  def /(d: Double) = Vector2(x / d, y / d)
  
  def unary_- = Vector2(-x, -y)
  
  def ~==(that: Vector2): Boolean = {
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
      new Vector2(f * x, f * y)
    } else {
      this
    }      
  }
  
  def lerp(that: Vector2, d: Double) = {
    val m = 1 - d
    Vector2(m * this.x + d * that.x, m * this.y + d * that.y)
  }
}

object Vector2 {
  val zero = Vector2
  val one = Vector2(1, 1)
  val negativeOne = Vector2(-1, -1)
  val unitX = Vector2(1, 0)
  val unitY = Vector2(0, 1)
  val negativeUnitX = Vector2(-1, 0)
  val negativeUnitY = Vector2(0, -1)
} 