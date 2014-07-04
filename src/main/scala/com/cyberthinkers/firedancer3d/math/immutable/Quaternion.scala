package com.cyberthinkers.firedancer3d.math.immutable
import org.scalajs.dom

/**
 * Quaternion
 *
 * @author Larry Melia
 * http://www.ogre3d.org/docs/api/1.9/classOgre_1_1Quaternion.html
 */
case class Quaternion(x: Double, y: Double, z: Double, w: Double) {
     def toEulerAngles() {
       val sqw = w * w
       val sqx = x * x
       val sqy = y * y
       val sqz = z * z
       val u = sqx + sqy + sqz + sqw
       val test = x * y + z * w
       if(test > 0.499) {
          (2 * Math.atan2(y, w), Quaternion.halfPI, 0)
       } else if(test < -0.499 * u) {
          (-2 * Math.atan2(y, w), Quaternion.halfPI, 0)
       }  else {
         (Math.atan2(2 * y * w - 2 * x * z, sqx - sqy -sqz + sqw), 
          Math.asin(2 * test / u), 
          Math.atan2(2 * x * w -2 * y * z, -sqx + sqy -sqz +sqw))
       }
     }

}

object Quaternion {
  
   val identity = new Quaternion(0, 0, 0, 1)
   val allowedDeviance = 0.00000001;
   val halfPI = Math.PI / 2
   
   /**
    * Create Quaternion from Euler angles (in radians)
    */
   def fromEulerAngles(heading: Double, attitude: Double, bank: Double) {
     val h = heading *.5
     val sinHeading = Math.sin(h)
     val cosHeading = Math.cos(h)
     val a = attitude * .5
     val sinAttitude = Math.sin(a)
     val cosAttitude = Math.cos(a)
     val b = bank *.5
     val sinBank = Math.sin(b)
     val cosBank = Math.cos(b)
     
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