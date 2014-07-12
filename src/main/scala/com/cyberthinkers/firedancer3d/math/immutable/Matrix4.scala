package com.cyberthinkers.firedancer3d.math.immutable

case class Matrix4 (
   m00: Double, m01: Double, m02: Double, m03: Double,
   m10: Double, m11: Double, m12: Double, m13: Double,
   m20: Double, m21: Double, m22: Double, m23: Double,
   m30: Double, m31: Double, m32: Double, m33: Double) {
  
  def this(m: Matrix4) = {
    this(m.m00, m.m01, m.m02, m.m03,
         m.m10, m.m11, m.m12, m.m13,
         m.m20, m.m21, m.m22, m.m23,
         m.m30, m.m31, m.m32, m.m33)
  }
  
  def this(m: Matrix3) = {
    this(m.m00, m.m01, m.m02, 0,
         m.m10, m.m11, m.m12, 0,
         m.m20, m.m21, m.m22, 0,
         0, 0, 0, 0)
  }
  
  def this(v: Vector3) = { // transpose matrix
    this(1, 0, 0, v.x,
         0, 1, 0, v.y,
         0, 0, 1, v.z,
         0, 0, 0, 1)
  }
  
  def rowTupled = {
    ((m00, m01, m02, m03),
     (m10, m11, m12, m13),
     (m20, m21, m22, m23),
     (m30, m31, m32, m33))
  }
  
  def columnTupled = {
    ((m00, m10, m20, m30),
     (m01, m11, m21, m31),
     (m02, m12, m22, m32),
     (m03, m13, m23, m33))
  }
  
  def transposeVector = {
    Vector3(m03, m13, m23)
  }
  
  def +(that: Matrix4) = {
    Matrix4(
      this.m00 + that.m00, this.m01 + that.m01, this.m02 + that.m02, this.m03 + that.m03,
      this.m10 + that.m10, this.m11 + that.m11, this.m12 + that.m12, this.m13 + that.m13,
      this.m20 + that.m20, this.m21 + that.m21, this.m22 + that.m22, this.m23 + that.m23,
      this.m30 + that.m30, this.m31 + that.m31, this.m32 + that.m32, this.m33 + that.m33)
  }
  
  def -(that: Matrix4) = {
    Matrix4(
      this.m00 - that.m00, this.m01 - that.m01, this.m02 - that.m02, this.m03 - that.m03,
      this.m10 - that.m10, this.m11 - that.m11, this.m12 - that.m12, this.m13 - that.m13,
      this.m20 - that.m20, this.m21 - that.m21, this.m22 - that.m22, this.m23 - that.m23,
      this.m30 - that.m30, this.m31 - that.m31, this.m32 - that.m32, this.m33 - that.m33)
  }

  def *(that: Vector4) = {
    Matrix4(
      this.m00 * that.x, this.m01 * that.y, this.m02 * that.z, this.m03 * that.w,
      this.m10 * that.x, this.m11 * that.y, this.m12 * that.z, this.m13 * that.w,
      this.m20 * that.x, this.m21 * that.y, this.m22 * that.z, this.m23 * that.w,
      this.m30 * that.x, this.m31 * that.y, this.m32 * that.z, this.m33 * that.w)
  }
  
  def determinant = {
      val v0 = m10 * m22 * m33 + m12 * m23 * m30 + m13 * m20 * m32 -
               m13 * m22 * m30 - m12 * m20 * m33 - m10 * m23 * m32
      val v1 = m10 * m21 * m33 + m11 * m23 * m30 + m13 * m20 * m31 -
               m13 * m21 * m30 - m11 * m20 * m33 - m10 * m23 * m31
      val v2 = m11 * m22 * m33 + m12 * m23 * m31 + m13 * m21 * m32 -
               m13 * m22 * m31 - m12 * m21 * m33 - m11 * m23 * m32
      val v3 = m10 * m21 * m32 + m11 * m22 * m30 + m12 * m20 * m31 -
               m12 * m21 * m30 - m11 * m20 * m32 - m10 * m22 * m31
      m00 * v2 - m01 * v0 + m02 * v1 - m03 * v3
  }
  
  def transpose = {
    Matrix4(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32,
      m03, m13, m23, m33)
  }
  
  def rotate(angle: Double, v: Vector3): Matrix4 = {
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
    Matrix4(n00, n01, n02, m03, n10, n11, n12, m13, n20, n21, n22, m23, m30, m31, m32, m33)
  }
  
  def rotateX(angle: Double): Matrix4 = {
    val cosAngle = Math.cos(angle)
    val sinAngle = Math.sin(angle)
    
    val n01 = m01 * cosAngle + m02 * sinAngle
    val n02 = m02 * cosAngle - m01 * sinAngle
    
    val n11 = m11 * cosAngle + m12 * sinAngle
    val n12 = m12 * cosAngle - m11 * sinAngle
    
    val n21 = m21 * cosAngle + m22 * sinAngle
    val n22 = m22 * cosAngle - m21 * sinAngle
    
    val n31 = m31 * cosAngle + m32 * sinAngle
    val n32 = m32 * cosAngle - m31 * sinAngle
    
    Matrix4(m00, n01, n02, m03, m10, n11, n12, m13, m20, n21, n22, m23, m30, n31, n32, m33)
  }
  
  def rotateY(angle: Double): Matrix4 = {
    val cosAngle = Math.cos(angle)
    val sinAngle = Math.sin(angle)
    
    val n00 = m00 * cosAngle - m02 * sinAngle
    val n02 = m00 * sinAngle + m02 * cosAngle
    
    val n10 = m10 * cosAngle - m12 * sinAngle
    val n12 = m10 * sinAngle + m12 * cosAngle
    
    val n20 = m20 * cosAngle - m22 * sinAngle
    val n22 = m20 * sinAngle + m22 * cosAngle
    
    val n30 = m30 * cosAngle - m32 * sinAngle
    val n32 = m30 * sinAngle + m32 * cosAngle
    
    Matrix4(n00, m01, n02, m03, n10, m11, n12, m13, n20, m21, n22, m23, n30, m31, n32, m33)
  }
  
  def rotateZ(angle: Double): Matrix4 = {
    val cosAngle = Math.cos(angle)
    val sinAngle = Math.sin(angle)
    
    val n00 = m00 * cosAngle + m01 * sinAngle
    val n01 = m01 * cosAngle - m00 * sinAngle
    
    val n10 = m10 * cosAngle + m11 * sinAngle
    val n11 = m11 * cosAngle - m10 * sinAngle
    
    val n20 = m20 * cosAngle + m21 * sinAngle
    val n21 = m21 * cosAngle - m20 * sinAngle
    
    val n30 = m30 * cosAngle + m31 * sinAngle
    val n31 = m31 * cosAngle - m30 * sinAngle
    
    Matrix4(n00, n01, m02, m03, n10, n11, m12, m13, n20, n21, m22, m23, n30, n31, m32, m33)
  }
}

object Matrix4 {
  val identity =
    Matrix4(1, 0, 0, 0,
            0, 1, 0, 0,
            0, 0, 1, 0,
            0, 0, 0, 1)
}