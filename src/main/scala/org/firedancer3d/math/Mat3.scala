package org.firedancer3d.math

import Math._
import org.firedancer3d.math.Vec3
import org.firedancer3d.math.EulerAngles

case class Mat3(
  m00: Double, m01: Double, m02: Double,
  m10: Double, m11: Double, m12: Double,
  m20: Double, m21: Double, m22: Double) {

  def this(m: Mat3) = {
    this(m.m00, m.m01, m.m02, m.m10, m.m11, m.m12, m.m20, m.m21, m.m22)
  }

  def this() = {
    this(Mat3.identity)
  }
  
  def tuplied = (m00, m01, m02, m10, m11, m12, m20, m21, m22)
  
  def +(that: Mat3): Mat3 = {
    Mat3(
      this.m00 + that.m00, this.m01 + that.m01, this.m02 + that.m02,
      this.m10 + that.m10, this.m11 + that.m11, this.m12 + that.m12,
      this.m20 + that.m20, this.m21 + that.m21, this.m22 + that.m22)
  }
  
  def -(that: Mat3): Mat3 = {
    Mat3(
      this.m00 - that.m00, this.m01 - that.m01, this.m02 - that.m02,
      this.m10 - that.m10, this.m11 - that.m11, this.m12 - that.m12,
      this.m20 - that.m20, this.m21 - that.m21, this.m22 - that.m22)
  }
  
  def *(that: Mat3): Mat3 = {
    Mat3(
      this.m00 * that.m00, this.m01 * that.m01, this.m02 * that.m02,
      this.m10 * that.m10, this.m11 * that.m11, this.m12 * that.m12,
      this.m20 * that.m20, this.m21 * that.m21, this.m22 * that.m22)
  }

  def *(that: Vec3): Mat3 = {
    Mat3(
      this.m00 * that.x, this.m01 * that.y, this.m02 * that.z,
      this.m10 * that.x, this.m11 * that.y, this.m12 * that.z,
      this.m20 * that.x, this.m21 * that.y, this.m22 * that.z)
  }
  
  def determinant: Double = {
      m00 * m11 * m22 + m01 * m12 * m20 + m02 * m10 * m21 -
      m02 * m11 * m20 - m01 * m10 * m22 - m00 * m12 * m21;
  }
  
  def transpose = {
    Mat3(
      m00, m10, m20,
      m01, m11, m21,
      m02, m12, m22)
  }
  
  def toEulerAngles = Mat3.toEulerAngles(this)
  
  def rotate(angle: Double, v: Vec3): Mat3 = {
    val cosAngle = Math.cos(angle)
    val sinAngle = Math.sin(angle)
    val fOneMinusCos = 1.0 - cosAngle;
    val fX2 = v.x * v.x
    val fY2 = v.y * v.y
    val fZ2 = v.z * v.z
    val fXYM = v.x * v.y * fOneMinusCos
    val fXZM = v.x * v.z * fOneMinusCos
    val fYZM = v.y * v.z * fOneMinusCos
    val fXSin = v.x * sinAngle
    val fYSin = v.y * sinAngle
    val fZSin = v.z * sinAngle
    val n00 = fX2 * fOneMinusCos + cosAngle;
    val n01 = fXYM - fZSin;
    val n02 = fXZM + fYSin;
    val n10 = fXYM + fZSin;
    val n11 = fY2 * fOneMinusCos + cosAngle;
    val n12 = fYZM - fXSin;
    val n20 = fXZM - fYSin;
    val n21 = fYZM + fXSin;
    val n22 = fZ2 * fOneMinusCos + cosAngle;
    Mat3(n00, n01, n02, n10, n11, n12, n20, n21, n22)
  }

  def rotateX(angle: Double): Mat3 = {
    val cosAngle = Math.cos(angle)
    val sinAngle = Math.sin(angle)
    
    val n01 = m01 * cosAngle + m02 * sinAngle
    val n02 = m02 * cosAngle - m01 * sinAngle
    
    val n11 = m11 * cosAngle + m12 * sinAngle
    val n12 = m12 * cosAngle - m11 * sinAngle
    
    val n21 = m21 * cosAngle + m22 * sinAngle
    val n22 = m22 * cosAngle - m21 * sinAngle
    
    Mat3(m00, n01, n02, m10, n11, n12, m20, n21, n22)
  }
  
  def rotateY(angle: Double): Mat3 = {
    val cosAngle = Math.cos(angle)
    val sinAngle = Math.sin(angle)
    
    val n00 = m00 * cosAngle - m02 * sinAngle
    val n02 = m00 * sinAngle + m02 * cosAngle
    
    val n10 = m10 * cosAngle - m12 * sinAngle
    val n12 = m10 * sinAngle + m12 * cosAngle
    
    val n20 = m20 * cosAngle - m22 * sinAngle
    val n22 = m20 * sinAngle + m22 * cosAngle 
    
    Mat3(n00, m01, n02, n10, m11, n12, n20, m21, n22)
  }
  
  def rotateZ(angle: Double): Mat3 = {
    val cosAngle = Math.cos(angle)
    val sinAngle = Math.sin(angle)
    
    val n00 = m00 * cosAngle + m01 * sinAngle
    val n01 = m01 * cosAngle - m00 * sinAngle
    
    val n10 = m10 * cosAngle + m11 * sinAngle
    val n11 = m11 * cosAngle - m10 * sinAngle
    
    val n20 = m20 * cosAngle + m21 * sinAngle
    val n21 = m21 * cosAngle - m20 * sinAngle

    Mat3(n00, n01, m02, n10, n11, m12, n20, n21, m22)
  }
}

object Mat3 {
  val identity = Mat3(1, 0, 0, 0, 1, 0, 0, 0, 1)

  def toEulerAngles(m: Mat3): EulerAngles = {
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