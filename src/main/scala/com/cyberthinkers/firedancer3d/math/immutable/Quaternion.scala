package com.cyberthinkers.firedancer3d.math.immutable
import math._

/**
 * Quaternion
 *
 * @author Larry Melia
 * http://www.ogre3d.org/docs/api/1.9/classOgre_1_1Quaternion.html
 */
case class Quaternion(w: Double, x: Double, y: Double, z: Double) {
  
     def this(q: Quaternion) = this(q.w, q.x, q.y, q.z)
     
     def this(v: (Double, Double, Double, Double)) = this(v._1, v._2, v._3, v._4)
     
     def this(w: Double, v: (Double, Double, Double)) = this(w, v._1, v._2, v._3)
     
     def this(w: Double, v: Vector3) = this(w, v.x, v.y, v.z)
     
     def this(eulerAngles: EulerAngles) = this(Quaternion.fromEulerAngles(eulerAngles))
     
  
     def tupled = (w, x, y, z)
     
     def *(that: Vector4) = {
       Quaternion(this.w * that.w, this.x * that.x, this.y * that.y, this.z * that.z)
     }
     
     def magnitudeSquared = w * w + x * x + y * y + z * z
     
     def magnitude = {
       magnitudeSquared match {
         case m if m > 0 => sqrt(magnitudeSquared)
         case _          => 1
       }    
     }
     
     def normalize: Quaternion = {
       val oneOverMag = 1 / magnitude
       Quaternion(w * oneOverMag, x * oneOverMag, y * oneOverMag, z * oneOverMag)
     }
   
     def rotate(angle: Double, xp: Double, yp: Double, zp: Double): Quaternion = {
       if(x == 0 && z == 0 && z == 0) {
          this
       }
       val halfAngle = .05 * angle
       val s = sin(halfAngle)
       val qw = cos(halfAngle)
       val qx = s * xp
       val qy = s * yp
       val qz = s * zp
       Quaternion(
         -x * qx - y * qy  -z * qz + w * qw, // w
          x * qw + y * qz - z * qy + w * qx, // x
         -x * qz + y * qw + z * qx + w * qy, // y
          x * qy - y * qx + z * qw + w * qz) // z
     }
     
     def rotateX(angle: Double): Quaternion = {
       val halfAngle = .05 * angle
       val s = sin(halfAngle)
       val c = cos(halfAngle)
       Quaternion(
         -x * s + w * c, // w
          x * c + w * s, // x
          y * c + z * s, // y
         -y * s + z * c) // z
     }
     
     def rotateY(angle: Double): Quaternion = {
       val halfAngle = .05 * angle
       val s = sin(halfAngle)
       val c = cos(halfAngle)
       Quaternion(
         -y * s + w * c, // w
          x * c - z * s, // x
          y * c + w * s, // y
          x * s + z * c) // z
     }    
     
     def rotateZ(angle: Double): Quaternion = {
       val halfAngle = .05 * angle
       val s = sin(halfAngle)
       val c = cos(halfAngle)
       Quaternion(
         -y * s + w * c, // w
          x * c - z * s, // x
          y * c + w * s, // y
          x * s + z * c) // z
     }
     
     
     def toEulerAngles(): EulerAngles = {
       val sqw = w * w
       val sqx = x * x
       val sqy = y * y
       val sqz = z * z
       val u = sqx + sqy + sqz + sqw
       x * y + z * w match {
         case x if x >  0.499     => EulerAngles(2 * atan2(y, w), Quaternion.halfPI, 0.0)
         case x if x < -0.499 * u => EulerAngles(-2 * atan2(y, w), Quaternion.halfPI, 0.0)
         case somethingElse       => 
           EulerAngles(
             atan2(2 * y * w - 2 * x * z, sqx - sqy -sqz + sqw), 
             asin(2 * somethingElse / u), 
             atan2(2 * x * w -2 * y * z, -sqx + sqy -sqz + sqw))
        }
     }
}

object Quaternion {
  
   val identity = new Quaternion(1, 0, 0, 0)
   val halfPI = Math.PI / 2
   
   def fromRotationMatrix(m: Matrix3): Quaternion = {
     // some code goes here
     
     identity
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
         d3 * cosBank - d4 * sinBank,
         d1 * cosBank - d2 * sinBank,
         d1 * sinBank + d2 * cosBank,
         d4 * cosBank + d3 * sinBank)
   }
     
}