package com.cyberthinkers.firedancer3d.math.immutable
import math._

/**
 * Quaternion
 *
 * @author Larry Melia
 */
case class Quaternion(x: Double, y: Double, z: Double, w: Double) {
  
     def this(q: Quaternion) = this(q.x, q.y, q.z, q.w)
     
     def this(v: (Double, Double, Double, Double)) = this(v._1, v._2, v._3, v._4)
     
     def this(w: Double, v: (Double, Double, Double)) = this(v._1, v._2, v._3, w)
     
     def this(w: Double, v: Vector3) = this(v.x, v.y, v.z, w)
     
     def this(eulerAngles: EulerAngles) = this(Quaternion.fromEulerAngles(eulerAngles))
     
     def tupled = (x, y, z, w)
     
     def *(that: Vector4) = {
       Quaternion(this.x * that.x, this.y * that.y, this.z * that.z, this.w * that.w)
     }
     
     def *(that: Quaternion) {
       Quaternion(this.x * that.x, this.y * that.y, this.z * that.z, this.w * that.w)
     }
     
     def unary_- = Quaternion(-x, -y, -z, -w)
     
     def magnitudeSquared =  x * x + y * y + z * z + w * w
     
     def magnitude = {
       magnitudeSquared match {
         case m if m > 0 => sqrt(m)
         case _          => 1
       }    
     }
     
     def normalize: Quaternion = {
       val f = 1 / magnitude
       Quaternion(x * f, y * f, z * f, w * f)
     }
   
     def rotate(angle: Double, xp: Double, yp: Double, zp: Double): Quaternion = {
       if(x == 0 && z == 0 && z == 0) {
          this
       }
       val halfAngle = .5 * angle
       val s = sin(halfAngle)
       val qw = cos(halfAngle)
       val qx = s * xp
       val qy = s * yp
       val qz = s * zp
       Quaternion(
          x * qw + y * qz - z * qy + w * qx, // x
         -x * qz + y * qw + z * qx + w * qy, // y
          x * qy - y * qx + z * qw + w * qz, // z
         -x * qx - y * qy  -z * qz + w * qw) // w
     }
     
     def rotateX(angle: Double): Quaternion = {
       val halfAngle = .5 * angle
       val s = sin(halfAngle)
       val c = cos(halfAngle)
       Quaternion(
          x * c + w * s, // x
          y * c + z * s, // y
         -y * s + z * c, // z
         -x * s + w * c) // w
     }
     
     def rotateY(angle: Double): Quaternion = {
       val halfAngle = .5 * angle
       val s = sin(halfAngle)
       val c = cos(halfAngle)
       Quaternion(
          x * c - z * s, // x
          y * c + w * s, // y
          x * s + z * c, // z
         -y * s + w * c) // w
     }    
     
     def rotateZ(angle: Double): Quaternion = {
       val halfAngle = .5 * angle
       val s = sin(halfAngle)
       val c = cos(halfAngle)
       Quaternion(     
          x * c - z * s, // x
          y * c + w * s, // y
          x * s + z * c, // z
         -y * s + w * c) // w
     }
     
     def objectToUprightToEulerAngles: EulerAngles = {
       val sp = -2 * (y * z - w * x)
       if(abs(sp) > .9999) { // if(gimbal lock)
         EulerAngles(
           Math.PI / 2 * sp,
           atan2(-x * z + w * y, .5 - y * y - z * z),
           0)
       } else {
         EulerAngles(
           asin(sp), 
           atan2(x * z + w * y, .5 - x * x - y * y),
           atan2(x * y + w * z, .5 - x * x - z * z))
       }
     }
     
     def uprightToObjectToEulerAngles: EulerAngles = {
       val sp = -2 * (y * z + w * x)
       if(abs(sp) > .9999) { // if(gimbal lock)
         EulerAngles(
           Math.PI / 2 * sp,
           atan2(-x * z - w * y, .5 - y * y - z * z),
           0)
       } else {
         EulerAngles(
           asin(sp), 
           atan2(x * z - w * y, .5 - x * x - y * y),
           atan2(x * y - w * z, .5 - x * x - z * z))
       }
     }
// original-  
//     def toEulerAngles(): EulerAngles = {
//       val sqx = x * x
//       val sqy = y * y
//       val sqz = z * z
//       val sqw = w * w
//       val u = sqx + sqy + sqz + sqw
//       x * y + z * w match {
//         case x if x >  0.499     => EulerAngles( 2 * atan2(y, w), Quaternion.halfPI, 0.0)
//         case x if x < -0.499 * u => EulerAngles(-2 * atan2(y, w), Quaternion.halfPI, 0.0)
//         case somethingElse       => 
//           EulerAngles(
//             atan2(2 * y * w - 2 * x * z, sqx - sqy -sqz + sqw), 
//             asin(2 * somethingElse / u), 
//             atan2(2 * x * w -2 * y * z, -sqx + sqy -sqz + sqw))
//        }
//     }
}

object Quaternion {
  
   val identity = new Quaternion(0, 0, 0, 1)
   
   def fromRotationMatrix(m: Matrix3): Quaternion = {
     // determine which of x,y,z,w is the largest
     val x = m.m00 - m.m11 - m.m22
     val y = m.m11 - m.m00 - m.m22
     val z = m.m22 - m.m00 - m.m11
     val w = m.m00 + m.m11 + m.m22
     val g = List(x, y, z, w)
     val index = g.indexOf(g.max)
     val max = sqrt(g(index) + 1) * .5
     val mult = .25 / max
     index match {
       case 0 => Quaternion( // x is largest
                   max,                   // x
                  (m.m01 + m.m10) * mult, // y
                  (m.m20 + m.m02) * mult, // z
                  (m.m12 - m.m21) * mult) // w
                  
       case 1 => Quaternion( // y is largest
                  (m.m01 + m.m10) * mult, // x
                   max,                   // y
                  (m.m12 + m.m21) * mult, // z
                  (m.m20 - m.m02) * mult) // w
                  
       case 2 => Quaternion( // z is largest
                  (m.m20 + m.m02) * mult, // x
                  (m.m12 + m.m21) * mult, // y
                   max,                   // z
                  (m.m01 - m.m10) * mult) // w
                 
       case 3 => Quaternion( // w is largest
                  (m.m12 - m.m21) * mult, // x
                  (m.m20 - m.m02) * mult, // y
                  (m.m01 - m.m10) * mult, // z
                   max)                   // w
     }
   }
   
   def slerp(q1: Quaternion, q2: Quaternion, t: Double): Quaternion = {
     val cosOmega = q1.w * q2.w + q1.x * q2.x + q1.y * q2.y + q1.z * q2.z
     val (cosOmega2, q3) = if(cosOmega < 0) (cosOmega, q2) else (-cosOmega, -q2)
     val (k0, k1) =
       if(cosOmega2 > .9999) { // if(need to prevent divide-by-zero)
         (1 - t, t)
       } else {
         val sinOmega = math.sqrt(1 - cosOmega2 * cosOmega2)
         val omega3 = atan2(sinOmega, cosOmega2)
         val f = 1 / sinOmega
         (sin((1 - t) * omega3) * f, sin(t * omega3 * f))
       }
     Quaternion( // interpolate
       q1.x * k0 + q3.x * k1,
       q1.y * k0 + q3.y * k1,
       q1.z * k0 + q3.z * k1,
       q1.w * k0 + q3.w * k1)
   }
   
   /**
    * Create Quaternion from Euler angles (in radians)
    */
   def fromEulerAngles(eulerAngles: EulerAngles): Quaternion = {
     val h = eulerAngles.heading * .5
     val sinHeading = sin(h)
     val cosHeading = cos(h)
     val a = eulerAngles.pitch *.5
     val sinAttitude = sin(a)
     val cosAttitude = cos(a)
     val b = eulerAngles.bank *.5
     val sinBank = sin(b)
     val cosBank = cos(b)
     
     val d1 = cosHeading * cosAttitude
     val d2 = sinHeading * sinAttitude
     val d3 = cosHeading * sinAttitude
     val d4 = sinHeading * cosAttitude
     
     Quaternion(
         d1 * cosBank - d2 * sinBank,
         d1 * sinBank + d2 * cosBank,
         d4 * cosBank + d3 * sinBank,
         d3 * cosBank - d4 * sinBank)
   }
}