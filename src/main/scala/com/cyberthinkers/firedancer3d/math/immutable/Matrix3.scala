package com.cyberthinkers.firedancer3d.math.immutable

import Math._

case class Matrix3(
  m00: Double, m01: Double, m02: Double,
  m10: Double, m11: Double, m12: Double,
  m20: Double, m21: Double, m22: Double) {

  def this(m: Matrix3) = {
    this(m.m00, m.m01, m.m02, m.m10, m.m11, m.m12, m.m20, m.m21, m.m22)
  }

  def toEulerAngles = Matrix3.toEulerAngles(this)
  
  def this() = {
    this(Matrix3.identity)
  }
}

object Matrix3 {
  val identity = Matrix3(1, 0, 0, 0, 1, 0, 0, 0, 1)

  def toEulerAngles(m: Matrix3): EulerAngles = {
    val sp = -m.m21
    val pitch = {
      if (sp <= -1) {
        -PI / 2
      } else if (sp >= 1) {
        PI / 2
      } else {
        asin(sp)
      }
    }
    if (abs(sp) > 0.9999) {
      EulerAngles(atan2(-m.m02, m.m00), pitch, 0)
    } else {
      EulerAngles(atan2(m.m20, m.m22), pitch, atan2(m.m01, m.m11))
    }
  }
}