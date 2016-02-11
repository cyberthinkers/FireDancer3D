package org.firedancer3d.scenegraph.geometry

import org.firedancer3d.math.Vec3

/**
 * Object is outside, inside or intersects the PolyhedronEnclosure
 */
object LocationOnPolyhedronEnclosure extends Enumeration {
  type LocationOnPolyhedronEnclosure = Value
  val outside, inside, intersects = Value
}

/**
 * PolyhedronEnclosure - area bound by planes where the normal of each plane points inward, usually a closed polyhedron.
 */
case class PolyhedronEnclosure(planes: Vector[Plane]) extends AnyVal {
  def pointInside(point: Vec3): Boolean = {
    if(planes.find(plane => (plane.whichSide(point) == LocationOnPlane.back)) == Some) false else true}
  
  def axisAlisgnedBoundingBoxPlanePositions(aabb: AxisAlignedBoundingBox) = for(plane <- planes) yield (plane, plane.whichSide(aabb))
  
  def axisAlisgnedBoundingBoxPosition(aabb: AxisAlignedBoundingBox) = {
     
  }
}