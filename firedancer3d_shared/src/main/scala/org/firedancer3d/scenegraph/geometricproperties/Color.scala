package org.firedancer3d.scenegraph.geometricproperties
import Math._
import scala.Vector

trait ColorSupport {
  protected def clamp0to1(v: Float): Float = if(v < 0) 0 else if(v > 1) 1 else v
  def vectored: Vector[Float]
}

case class ColorRGB(red: Float, green: Float, blue: Float) extends ColorSupport {
  def this(color: ColorRGBA) = this(color.red, color.green, color.blue)
  
  def this(rgb: Int) =
    this(((rgb & 0x00ff0000) >>> 16) / 255f, ((rgb & 0x0000ff00) >>> 8) / 255f, (rgb & 0x000000ff) / 255f)

  def toIntRGB(): Int = ((red.toInt * 255) << 16) + ((green.toInt * 255) << 8) + (blue.toInt * 255)
    
  def clamp = ColorRGBA(clamp0to1(red), clamp0to1(green), clamp0to1(blue))
  
  def luminance = red * 0.3 + green * 0.59 + blue *  0.11
  
  def vectored = Vector(red, green, blue)
  
  def +(that: ColorRGBA) = ColorRGBA(this.red + that.red, this.green + that.green, this.blue + that.blue)
  def -(that: ColorRGBA) = ColorRGBA(this.red - that.red, this.green - that.green, this.blue - that.blue)
  def *(that: ColorRGBA) = ColorRGBA(this.red * that.red, this.green * that.green, this.blue * that.blue)
  def /(that: ColorRGBA) = ColorRGBA(this.red / that.red, this.green / that.green, this.blue / that.blue)
  
  def *(scale: Float) = ColorRGBA(red * scale, green * scale, blue * scale)
  def /(scale: Float) = ColorRGBA(red / scale, green / scale, blue / scale)
  
  def lerp(endColor: ColorRGBA, scalar: Float) {
    val v = 1.0f - scalar
    ColorRGBA(
      v * red   + scalar * endColor.red,
      v * green + scalar * endColor.green,
      v * blue  + scalar * endColor.blue)
  }
  
  def toColorHSLA: ColorHSLA = {
    val max = if(red > green && red > blue) red else if(green > blue) green else blue
    val min = if(red < green && red < blue) red else if(green < blue) green else blue
    val lightness = (min + max) / 2
    if(min == max) {
      ColorHSLA(0, 0, lightness)
    } else {
      val delta = max - min
      val saturation = if(lightness > .5) delta / (2 - max - min) else delta / (max + min)
      val h: Float =
        if(red > green && red > blue) {
          (green - blue) / delta + (if(green < blue) 6 else 0)
        } else if(green > blue) {
          (blue - red) / delta + 2
        } else {
          (red - green) / delta + 4
        }
      val hue = h / 6
      ColorHSLA(hue, saturation, lightness)
    }
  }
}

case class ColorRGBA(red: Float, green: Float, blue: Float, alpha: Float = 1) extends ColorSupport {
  def this(color: ColorRGBA) = this(color.red, color.green, color.blue, color.alpha)
  
  def this(color: ColorRGB) = this(color.red, color.green, color.blue)
  
  def this(rgba: Int) =
    this(((rgba & 0xff000000) >>> 24) / 255f, ((rgba & 0x00ff0000) >>> 16) / 255f, 
         ((rgba & 0x0000ff00) >>> 8) / 255f, (rgba & 0x000000ff) / 255f)
     
  def toIntRGbA(): Int = ((red.toInt * 255) << 16) + ((green.toInt * 255) << 8) + (blue.toInt * 255)

  def clamp = ColorRGBA(clamp0to1(red), clamp0to1(green), clamp0to1(blue), clamp0to1(alpha))
  
  def vectored = Vector(red, green, blue, alpha)
  
  def +(that: ColorRGBA) = ColorRGBA(this.red + that.red, this.green + that.green, this.blue + that.blue, this.alpha + that.alpha)
  def -(that: ColorRGBA) = ColorRGBA(this.red - that.red, this.green - that.green, this.blue - that.blue, this.alpha - that.alpha)
  def *(that: ColorRGBA) = ColorRGBA(this.red * that.red, this.green * that.green, this.blue * that.blue, this.alpha * that.alpha)
  def /(that: ColorRGBA) = ColorRGBA(this.red / that.red, this.green / that.green, this.blue / that.blue, this.alpha / that.alpha)
  
  def *(scale: Float) = ColorRGBA(red * scale, green * scale, blue * scale, alpha * scale)
  def /(scale: Float) = ColorRGBA(red / scale, green / scale, blue / scale, alpha / scale)
  
  def lerp(endColor: ColorRGBA, scalar: Float) {
    val v = 1.0f - scalar
    ColorRGBA(
      v * red   + scalar * endColor.red,
      v * green + scalar * endColor.green,
      v * blue  + scalar * endColor.blue,
      v * alpha + scalar * endColor.alpha)
  }
  
  def toColorHSLA: ColorHSLA = {
    val max = if(red > green && red > blue) red else if(green > blue) green else blue
    val min = if(red < green && red < blue) red else if(green < blue) green else blue
    val lightness = (min + max) / 2
    if(min == max) {
      ColorHSLA(0, 0, lightness, alpha)
    } else {
      val delta = max - min
      val saturation = if(lightness > .5) delta / (2 - max - min) else delta / (max + min)
      val h: Float =
        if(red > green && red > blue) {
          (green - blue) / delta + (if(green < blue) 6 else 0)
        } else if(green > blue) {
          (blue - red) / delta + 2
        } else {
          (red - green) / delta + 4
        }
      val hue = h / 6
      ColorHSLA(hue, saturation, lightness, alpha)
    }
  }
}

case class ColorHSLA(hue: Float, saturation: Float, lightness: Float, alpha: Float = 1) extends ColorSupport {
  def this(color: ColorHSLA) = this(color.hue, color.saturation, color.lightness, color.alpha)

  def vectored = Vector(hue, saturation, lightness, alpha)

  def toColorRGBA() = {
    if(saturation == 0) {
      ColorRGBA(1, 1, 1, alpha)
    } else {
      def hue2rgb(p: Float, q: Float, t: Float) = {
        val r = if(t < 0) t + 1 else if(t > 0) t - 1 else t
        if(r < 1.0f/6.0f) p + (q - p) * 6 * r
        else if(r < 1.0f/2.0f) q
        else if(r < 2.0f/3.0f) p + (q - p) * (2.0f/3.0f - r) * 6
        else p
      }
      val q = if(lightness < .5f) lightness * (lightness + saturation) else lightness + saturation - lightness * saturation
      val p = 2 * lightness - q
      ColorRGBA(hue2rgb(p, q, hue + 1.0f/3.0f), hue2rgb(p, q, hue), hue2rgb(p, q, hue - 1.0f/3.0f), alpha)
    }
  }
    def toColorRGB() = {
    if(saturation == 0) {
      ColorRGB(1, 1, 1)
    } else {
      def hue2rgb(p: Float, q: Float, t: Float) = {
        val r = if(t < 0) t + 1 else if(t > 0) t - 1 else t
        if(r < 1.0f/6.0f) p + (q - p) * 6 * r
        else if(r < 1.0f/2.0f) q
        else if(r < 2.0f/3.0f) p + (q - p) * (2.0f/3.0f - r) * 6
        else p
      }
      val q = if(lightness < .5f) lightness * (lightness + saturation) else lightness + saturation - lightness * saturation
      val p = 2 * lightness - q
      ColorRGB(hue2rgb(p, q, hue + 1.0f/3.0f), hue2rgb(p, q, hue), hue2rgb(p, q, hue - 1.0f/3.0f))
    }
  }
}

