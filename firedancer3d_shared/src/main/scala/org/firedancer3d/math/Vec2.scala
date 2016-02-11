package org.firedancer3d.math

import Math._

case class Vec2(x:Double, y:Double) extends Vectored {

  def this(v: Vec2) = this(v.x, v.y)
  
  def vectored = Vector(x, y)
  
  def +(that: Vec2) = Vec2(this.x + that.x, this.y + that.y)
  def -(that: Vec2) = Vec2(this.x - that.x, this.y - that.y)
  def *(that: Vec2) = Vec2(this.x * that.x, this.y * that.y)
  def /(that: Vec2) = Vec2(this.x / that.x, this.y / that.y)
  
  def *(d: Double) = Vec2(x * d, y * d)
  def /(d: Double) = Vec2(x / d, y / d)
  
  def unary_- = Vec2(-x, -y)
  
  def ==~(that: Vec2): Boolean = {
    abs(this.x - that.x) < epsilon &&
    abs(this.y - that.y) < epsilon 
  }
  
  def length = Math.sqrt(lengthSquared)
  
  def lengthSquared = x * x + y * y
  
  def normalize = {
    val d = lengthSquared
    if(d > 0 && d < 1) {
      val f = 1.0 / Math.sqrt(d)
      new Vec2(f * x, f * y)
    } else this     
  }
  
  def lerp(that: Vec2, d: Double) = {
    val m = 1 - d
    Vec2(m * this.x + d * that.x, m * this.y + d * that.y)
  }
}

object Vec2 {
  val zero = Vec2
  val one = Vec2(1, 1)
  val negativeOne = Vec2(-1, -1)
  val unitX = Vec2(1, 0)
  val unitY = Vec2(0, 1)
  val negativeUnitX = Vec2(-1, 0)
  val negativeUnitY = Vec2(0, -1)
} 