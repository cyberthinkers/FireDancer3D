package example

import scala.scalajs.js
import js.annotation.JSExport
import scalatags.Text.all._
import org.scalajs.dom
import scala.Vector
import org.firedancer3d.math._

object ScalaJSExample extends js.JSApp {
  def main(): Unit = {
    val v = Vec3(1.0, 2.0, 3.0)
    
    //val v = Vector(1.0, 2.0, 3.0)
    val paragraph = dom.document.createElement("p")
    val v2 = v * 3
    paragraph.innerHTML = s"Hey: <strong>It works! $v2</strong>"
    dom.document.getElementById("playground").appendChild(paragraph)
  }
}
