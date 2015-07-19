package example

import scala.scalajs.js
import js.annotation.JSExport
import scalatags.Text.all._
import org.scalajs.dom
import scala.Vector
import org.firedancer3d.math._
import org.firedancer3d.surface._
import org.scalajs.dom
import org.scalajs.dom.{html, webgl}

object ScalaJSExample extends js.JSApp {
  
  def isValid(v: Vector[Double]): Boolean =
     if(v.find(d => (d equals Double.NaN)  || (d equals Double.PositiveInfinity) || (d equals Double.NegativeInfinity)) == Some) false else true

//  def dot(v1:Vector[Double], v2:Vector[Double]) = {(v1, v2).zipped.map(_*_).sum}
  
  def main(): Unit = {

//    val v1 = Vector(1.0,2.0,3.0)
//    val v2 = Vector(4.0,5.0,6.0)
//    val v = dot(v1, v2)
    val vv1 = Vec3(1,2,3)
    val vv2 = Vec3(4,5,6)
    val vv = vv1 dot vv2
    val paragraph = dom.document.createElement("p")
    paragraph.innerHTML = s"What's up?: <strong>$vv</strong>"
    dom.document.getElementById("playground").appendChild(paragraph)
    val canvas = dom.document.getElementById("glcanvas").asInstanceOf[html.Canvas]
    val renderingContext = WebGLCanvas.getRenderingContext(canvas)
    println(renderingContext)
  }
}
