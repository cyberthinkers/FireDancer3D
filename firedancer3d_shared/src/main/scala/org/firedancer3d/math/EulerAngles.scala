package org.firedancer3d.math

import Math._

case class EulerAngles(heading: Double, pitch: Double, bank: Double) {
   def this(e: EulerAngles) = this(e.heading, e.pitch, e.bank)
   def toCanonicalForm: EulerAngles = EulerAngles.toCanonicalForm(heading, pitch, bank)
}

object EulerAngles {
  val identity =  EulerAngles(0, 0, 0)
 
  // this may not be needed here and it needs some restructuring
  def toCanonicalForm(heading: Double, pitch: Double, bank: Double): EulerAngles = {
    val pitchWrapped = Utilities.wrapPI(pitch)
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
    if(abs(c.pitch) > (PI / 2) - 1e-4) { 
      EulerAngles(Utilities.wrapPI(c.heading + c.bank), c.pitch, 0)
    } else {
      EulerAngles(Utilities.wrapPI(c.heading), c.pitch, Utilities.wrapPI(c.bank))
    }
  }
}