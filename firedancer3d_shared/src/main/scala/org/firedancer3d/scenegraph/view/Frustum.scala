package org.firedancer3d.scenegraph.view

import org.firedancer3d.scenegraph.geometry.Plane

/**
 * Object is outside, inside or intersects the Frustum
 */
object LocationOnFrustum extends Enumeration {
  type LocationOnFrustum = Value
  val outside, inside, intersects = Value
}

object PlanesOfTheFrustum extends Enumeration {
  type PlanesOfTheFrustum = Value
  val near, far, left, right, bottom, top = Value
}

case class Frustum(planes: Vector[Plane] = Vector(Plane(), Plane(), Plane(), Plane(), Plane(), Plane())) {
//  def this(fovY: Double, aspect: Double, near: Double, far: Double) {
//    val h = Math.tan(Math.toRadians(degforY))
//  }
}

object Frustrum {
  def frustrumPerspective = {
    
  }
}