package org.firedancer3d.math

import Math._

case class Vec3(x:Double, y:Double, z:Double) extends Vectored {

  def this(v: Vec3) = this(v.x, v.y, v.z)

  def this(t: (Double, Double, Double)) = this(t._1, t._2, t._3)

  def vectored = Vector(x, y, z)
  
  def +(that: Vec3) = Vec3(this.x + that.x, this.y + that.y, this.z + that.z)
  def -(that: Vec3) = Vec3(this.x - that.x, this.y - that.y, this.z - that.z)
  def *(that: Vec3) = Vec3(this.x * that.x, this.y * that.y, this.z * that.z)
  def /(that: Vec3) = Vec3(this.x / that.x, this.y / that.y, this.z / that.z)
  
  def *(d: Double) = Vec3(x * d, y * d, z * d)
  def /(d: Double) = Vec3(x / d, y / d, z / d)
  
  /**
   * Negate
   */
  def unary_- = Vec3(-x, -y, -z)
  
  def ==~(that: Vec3): Boolean = {
    val epsilon = (1e-4)
    abs(this.x - that.x) < epsilon &&
    abs(this.y - that.y) < epsilon &&
    abs(this.z - that.z) < epsilon 
  }
  
  /**
   * Dot product
   */
  def dot(that: Vec3): Double = {this.x * that.x + this.y * that.y + this.z * that.z}

  def crossProduct(that: Vec3) = Vec3(this.y * that.z - this.z * that.y,
                                            this.z * that.x - this.x * that.z,
                                            this.x * that.y - this.y * that.x)
  
  def length = Math.sqrt(lengthSquared)
  
  def lengthSquared = x * x + y * y + z * z
  
  def normalize = {
    val d = length
    if(d > 0.0) {
      val f = 1.0 / d
      new Vec3(f * x, f * y, f * z)
    } else {
      this
    }      
  }
  
  def lerp(that: Vec3, d: Double) = {
    val m = 1 - d
    Vec3(m * this.x + d * that.x, m * this.y + d * that.y, m * this.z + d * that.z)
  }
}

object Vec3 {
  val zero = Vec3(0, 0 ,0)
  val one = Vec3(1, 1, 1)
  val negativeOne = Vec3(-1, -1, -1)
  val unitX = Vec3(1, 0, 0)
  val unitY = Vec3(0, 1, 0)
  val unitZ = Vec3(0, 0, 1)
  val negativeUnitX = Vec3(-1, 0, 0)
  val negativeUnitY = Vec3(0, -1, 0)
  val negativeUnitZ = Vec3(0, 0, -1)
  
  def min(v1: Vec3, v2: Vec3): Vec3 = {
    Vec3(
      if(v1.x < v2.x) v1.x else v2.x,
      if(v1.y < v2.y) v1.y else v2.y,
      if(v1.z < v2.z) v1.z else v2.z)
  }
  
  def max(v1: Vec3, v2: Vec3): Vec3 = {
    Vec3(
      if(v1.x > v2.x) v1.x else v2.x,
      if(v1.y > v2.y) v1.y else v2.y,
      if(v1.z > v2.z) v1.z else v2.z)
  }
} 