package com.cyberthinkers.firedancer3d.math

import Math._

case class Vector3(x:Double, y:Double, z:Double) extends Vectored {

  def this(v: Vector3) = this(v.x, v.y, v.z)

  def this(t: (Double, Double, Double)) = this(t._1, t._2, t._3)

  def vectored = Vector(x, y, z)
  
  def +(that: Vector3) = Vector3(this.x + that.x, this.y + that.y, this.z + that.z)
  def -(that: Vector3) = Vector3(this.x - that.x, this.y - that.y, this.z - that.z)
  def *(that: Vector3) = Vector3(this.x * that.x, this.y * that.y, this.z * that.z)
  def /(that: Vector3) = Vector3(this.x / that.x, this.y / that.y, this.z / that.z)
  
  def *(d: Double) = Vector3(x * d, y * d, z * d)
  def /(d: Double) = Vector3(x / d, y / d, z / d)
  
  /**
   * Negate
   */
  def unary_- = Vector3(-x, -y, -z)
  
  def ==~(that: Vector3): Boolean = {
    val epsilon = (1e-4)
    abs(this.x - that.x) < epsilon &&
    abs(this.y - that.y) < epsilon &&
    abs(this.z - that.z) < epsilon 
  }
  
  /**
   * Dot product
   */
  def dot(that: Vector3): Double = {this.x * that.x + this.y * that.y + this.z * that.z}

  def crossProduct(that: Vector3) = Vector3(this.y * that.z - this.z * that.y,
                                            this.z * that.x - this.x * that.z,
                                            this.x * that.y - this.y * that.x)
  
  def length = Math.sqrt(lengthSquared)
  
  def lengthSquared = x * x + y * y + z * z
  
  def normalize = {
    val d = length
    if(d > 0.0) {
      val f = 1.0 / d
      new Vector3(f * x, f * y, f * z)
    } else {
      this
    }      
  }
  
  def lerp(that: Vector3, d: Double) = {
    val m = 1 - d
    Vector3(m * this.x + d * that.x, m * this.y + d * that.y, m * this.z + d * that.z)
  }
}

object Vector3 {
  val zero = Vector3(0, 0 ,0)
  val one = Vector3(1, 1, 1)
  val negativeOne = Vector3(-1, -1, -1)
  val unitX = Vector3(1, 0, 0)
  val unitY = Vector3(0, 1, 0)
  val unitZ = Vector3(0, 0, 1)
  val negativeUnitX = Vector3(-1, 0, 0)
  val negativeUnitY = Vector3(0, -1, 0)
  val negativeUnitZ = Vector3(0, 0, -1)
  
  def min(v1: Vector3, v2: Vector3): Vector3 = {
    Vector3(
      if(v1.x < v2.x) v1.x else v2.x,
      if(v1.y < v2.y) v1.y else v2.y,
      if(v1.z < v2.z) v1.z else v2.z)
  }
  
  def max(v1: Vector3, v2: Vector3): Vector3 = {
    Vector3(
      if(v1.x > v2.x) v1.x else v2.x,
      if(v1.y > v2.y) v1.y else v2.y,
      if(v1.z > v2.z) v1.z else v2.z)
  }
} 