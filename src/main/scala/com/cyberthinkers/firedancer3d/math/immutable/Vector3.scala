package com.cyberthinkers.firedancer3d.math.immutable

case class Vector3(x:Double, y:Double, z:Double) {

  def this(v: Vector3) {
    this(v.x, v.y, v.z)
  }

  def this(t: (Double, Double, Double)) {
    this(t._1, t._2, t._3)
  }

  def +(that: Vector3) = Vector3(this.x + that.x, this.y + that.y, this.z + that.z)
  def -(that: Vector3) = Vector3(this.x - that.x, this.y - that.y, this.z - that.z)
  def *(that: Vector3) = Vector3(this.x * that.x, this.y * that.y, this.z * that.z)
  def /(that: Vector3) = Vector3(this.x / that.x, this.y / that.y, this.z / that.z)
  
  def *(d: Double) = Vector3(x * d, y * d, z * d)
  def /(d: Double) = Vector3(x / d, y / d, z / d)
  
  def - = Vector3(-x, -y, -z)
  
  def tupled = (x, y, z)
  
  def length = {
    Math.sqrt(lengthSquared)
  }
  
  def lengthSquared = {
    x * x + y * y + z * z
  }
  
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
  val zero = Vector3
  val one = Vector3(1, 1, 1)
  val negativeOne = Vector3(-1, -1, -1)
  val unitX = Vector3(1, 0, 0)
  val unitY = Vector3(0, 1, 0)
  val unitZ = Vector3(0, 0, 1)
  val negativeUnitX = Vector3(-1, 0, 0)
  val negativeUnitY = Vector3(0, -1, 0)
  val negativeUnitZ = Vector3(0, 0, -1)
} 