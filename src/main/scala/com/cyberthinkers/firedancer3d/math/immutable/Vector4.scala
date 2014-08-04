package com.cyberthinkers.firedancer3d.math.immutable

import Math._

case class Vector4(x:Double, y:Double, z:Double, w:Double) extends Vectored {

  def this(v: Vector4) = this(v.x, v.y, v.z, v.w)

  def this(t: (Double, Double, Double, Double)) = this(t._1, t._2, t._3, t._4)
      
  def vectored = Vector(x, y, z, w)
  
  def +(that: Vector4) = Vector4(this.x + that.x, this.y + that.y, this.z + that.z, this.w + that.w)
  def -(that: Vector4) = Vector4(this.x - that.x, this.y - that.y, this.z - that.z, this.w - that.w)
  def *(that: Vector4) = Vector4(this.x * that.x, this.y * that.y, this.z * that.z, this.w * that.w)
  def /(that: Vector4) = Vector4(this.x / that.x, this.y / that.y, this.z / that.z, this.w / that.w)
  
  def *(d: Double) = Vector4(x * d, y * d, z * d, w * d)
  def /(d: Double) = Vector4(x / d, y / d, z / d, w / d)
  
  def unary_- = Vector4(-x, -y, -z, -w)
  
  def ~==(that: Vector4): Boolean = {
    val epsilon = (1e-4)
    abs(this.x - that.x) < epsilon &&
    abs(this.y - that.y) < epsilon &&
    abs(this.z - that.z) < epsilon &&
    abs(this.w - that.w) < epsilon
  }
  
  def lengthSquared = x * x + y * y + z * z + w * w
  
  def length = Math.sqrt(lengthSquared)
  
  def normalize = {
    val d = length
    if(d > 0.0) {
      val f = 1.0 / d
      new Vector4(f * x, f * y, f * z, f * w)
    } else {
      this
    }      
  }
  
  def lerp(that: Vector4, d: Double) = {
    val m = 1 - d
    Vector4(m * this.x + d * that.x, m * this.y + d * that.y, m * this.z + d * that.z, m * this.w + d * that.w)
  }

  //override lazy val hashCode: Int = vector.hashCode
}

object Vector4 {
  val zero = Vector4
  val one = Vector4(1, 1, 1, 1)
  val negativeOne = Vector4(-1, -1, -1, -1)
  val unitX = Vector4(1, 0, 0, 0)
  val unitY = Vector4(0, 1, 0, 0)
  val unitZ = Vector4(0, 0, 1, 0)
  val unitW = Vector4(0, 0, 0, 1)
  val negativeUnitX = Vector4(-1, 0, 0, 0)
  val negativeUnitY = Vector4(0, -1, 0, 0)
  val negativeUnitZ = Vector4(0, 0, -1, 0)
  val negativeUnitW = Vector4(0, 0, 0, -1)
} 