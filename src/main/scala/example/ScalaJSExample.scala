package example

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import com.cyberthinkers.firedancer3d.math._
import com.cyberthinkers.firedancer3d.math.immutable._
import com.cyberthinkers.firedancer3d.math.Vector4
import com.cyberthinkers.firedancer3d.math.Vector3
import com.cyberthinkers.firedancer3d.math.Matrix4
import com.cyberthinkers.firedancer3d.math.EulerAngles
import com.cyberthinkers.firedancer3d.math.ColorRGBA

object ScalaJSExample extends js.JSApp {
  def main(): Unit = {
    val c = ColorRGBA(.5f,.5f,.5f,.5f)
    val t = c.toColorHSLA
    //val v = Matrix4(); // doesn't work
    val tmp = EulerAngles.toCanonicalForm(.3, .2, .1)
    val m = Matrix4.identity
    val m1 = m.rotate(.3, Vector3(.1,.2,.3))
    var d = new js.Date()
    var v4 = new Vector4(1,2,3,4)

    println("start test " + d.getTime)
    var n = 0.0
    var h = 0
    var ts1 = new js.Date
//    for(v <- 1 to 100000000) {
//      v4 = new Vector4(1,2,3,4)
//      //n = v4.length
//      //h = v4.hashCode
//    }
    var ts2 = new js.Date
    var u1 = ts1.getTime
    var u2 = ts2.getTime
    println(s"end of test n=$n hash=$h $u1 $u2 ${u2 - u1}")
    
//    var m3 = Matrix3()
//    var v3 = Vector3(.2,.3, .4)
//    ts1 = new js.Date
//    for(v <- 1 to 10000000) {
//        m3.fromAngleNormalAxis(.3, v3)
//    }
//     ts2 = new js.Date
    u1 = ts1.getTime
    u2 = ts2.getTime
     println(s"end of test2 $u1 $u2 ${u2 - u1}")
    val paragraph = dom.document.createElement("p")
    paragraph.innerHTML = "<strong>It works!</strong>"
    dom.document.getElementById("playground").appendChild(paragraph)
  }

  /** Computes the square of an integer.
   *  This demonstrates unit testing.
   */
  def square(x: Int): Int = x*x
}
