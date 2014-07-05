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
   
   /**
    * Create Quaternion from Euler angles (in radians)
    */
   def fromEulerAngles(eulerAngles: EulerAngles) = {
     val h = eulerAngles.heading *.5
     val sinHeading = sin(h)
     val cosHeading = cos(h)
     val a = eulerAngles.attitude * .5
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