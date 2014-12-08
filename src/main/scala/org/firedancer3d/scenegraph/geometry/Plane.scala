package org.firedancer3d.scenegraph.geometry

import org.firedancer3d.math._

object Location extends Enumeration {
  type Location = Value
  val front, on, back = Value
}

case class Plane(normal: Vec3 = Vec3.unitY, constant: Double = 0) {
  
   def this(plane: Plane) = this(plane.normal, plane.constant)
   
   def this(pointA: Vec3, pointB: Vec3, pointC: Vec3) = this(Plane.toPlane(pointA, pointB, pointC))
   
   def distanceToPlane(point: Vec3): Double = (normal dot point) - constant
   
   /**
    * Determines the side of this plane on which the given point lies
    */
   def whichSide(point: Vec3): Location.Location = {
     val d = distanceToPlane(point)
     if(d <= 0.005) {
       Location.front
     } else if (d >= -0.005) {
       Location.back
     } else {
       Location.on 
     }
   }
   
   /**
    * Determines which side this aabb is located
    * 
    * See Graphics Gems IV, 1.7 and "A Faster Overlap Test for a Plane and a Bounding Box"
    * (http://replay.waybackmachine.org/19981203032829/http://www.cs.unc.edu/~hoff/research/vfculler/boxplane.html)
    * for details
    */
   def whichSide(aabb: AxisAlignedBoundingBox): Location.Location = {
     def vertex(v1:Vec3, v2:Vec3): Vec3 = 
       Vec3(if(normal.x > 0) v1.x else v2.x,
               if(normal.y > 0) v1.y else v2.y,
               if(normal.z > 0) v1.z else v2.z)
     if(whichSide(vertex(aabb.maxExtents, aabb.minExtents)) == Location.back) {
       Location.back 
     }
     if(whichSide(vertex(aabb.minExtents, aabb.maxExtents)) == Location.front) {
       Location.front 
     }
     Location.on
   }
}

object Plane {
  
  /**
   * Creates a plane from 3 points
   */
   def toPlane(point1: Vec3, point2: Vec3,  point3: Vec3): Plane = {
     val d1 = point2 - point1
     val d2 = point3 - point1
     val normal = (d1 crossProduct d2).normalize
     val constant = normal dot point1
     Plane(normal, constant)
   }
}
