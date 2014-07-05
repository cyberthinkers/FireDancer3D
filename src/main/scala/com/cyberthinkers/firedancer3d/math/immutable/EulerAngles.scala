package com.cyberthinkers.firedancer3d.math.immutable
import Math._
//
// Yaw, Pitch, Roll is the same as Heading, Pitch, Bank
case class EulerAngles(heading: Double, attitude: Double, bank: Double) {
   def toCanonicalForm: EulerAngles = EulerAngles.toCanonicalForm(heading, attitude, bank)

}

object EulerAngles {
  val identity =  EulerAngles(0, 0, 0)
  
  private[this] def wrapPI(theta: Double): Double = (floor((theta + PI) * (1 / PI)) * PI * 2) - PI
  
  def toCanonicalForm(heading: Double, pitch: Double, bank: Double): EulerAngles = {
    val pitchWrapped = wrapPI(pitch)
    val c =
      if(pitchWrapped < (-PI / 2) ) { 
         EulerAngles(-PI - pitchWrapped, heading + PI, bank + PI) 
      } else if(pitchWrapped > (PI / 2)){
         EulerAngles(PI - pitchWrapped, heading + PI, bank + PI)
      } else {
         EulerAngles(heading, pitchWrapped, bank)
      }
    // if(Gimbal lock is detected) 
    //   assign all rotation about the vertical axis to heading
    // else
    //   just wrap the bank angle in canonical range
    if(abs(c.attitude) > (PI / 2) - 1e-4) { 
      EulerAngles(wrapPI(c.heading + c.bank), c.attitude, 0)
    } else {
      EulerAngles(wrapPI(c.heading), c.attitude, wrapPI(c.bank))
    }
  }
}