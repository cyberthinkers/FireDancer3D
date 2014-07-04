package com.cyberthinkers.firedancer3d.math.immutable

case class Matrix3 (
   m00: Double, m01: Double, m02: Double,
   m10: Double, m11: Double, m12: Double,
   m20: Double, m21: Double, m22: Double) {
  
  def this(m: Matrix3) {
    this(m.m00, m.m01, m.m02, m.m10, m.m11, m.m12, m.m20, m.m21, m.m22)
  }

  def this() {
    this(Matrix3.identity)
  }
}

object Matrix3 {
  val identity = Matrix3(1, 0, 0, 0, 1, 0, 0, 0, 1)
}