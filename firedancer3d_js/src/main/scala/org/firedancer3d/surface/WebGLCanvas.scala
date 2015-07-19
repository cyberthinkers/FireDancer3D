package org.firedancer3d.surface

import org.scalajs.dom
import org.scalajs.dom.{html, webgl}
import scala.scalajs.js.annotation.JSExport

class WebGLCanvas {

}

object WebGLCanvas  {
  def getRenderingContext(canvas: html.Canvas): webgl.RenderingContext = {
    var gl = canvas.getContext("webgl").asInstanceOf[webgl.RenderingContext]
    if(gl == null) gl = canvas.getContext("experimental-webgl").asInstanceOf[webgl.RenderingContext]
    gl
  }
}