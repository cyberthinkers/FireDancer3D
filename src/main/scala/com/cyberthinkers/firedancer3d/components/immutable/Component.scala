package com.cyberthinkers.firedancer3d.components.immutable

import com.cyberthinkers.firedancer3d.math.immutable._

abstract class Component

case class Ray(origin: Vector3 = Vector3.zero, direction: Vector3 = Vector3.unitZ) extends Component {
  def distanceSquared(point: Vector3) = {
    val p1 = point - origin
    val t0 = direction dot p1
    val p2 = direction * t0 + origin
    val p3 = point - p2
    p3.lengthSquared
  }
}

object PointLocation extends Enumeration {
  type PointLocation = Value
  val inside, outside, neither = Value
}

case class Plane(normal: Vector3 = Vector3.unitY, constant: Double = 0) extends Component {
  
   def this(plane: Plane) = this(plane.normal, plane.constant)
   
   def this(pointA: Vector3, pointB: Vector3, pointC: Vector3) = this(Plane.toPlain(pointA, pointB, pointC))
   
   def pseudoDistance(point: Vector3): Double = (normal dot point) - constant
   
   /**
    * Determines the side of this plane on which the given point lies
    */
   def pointLocation(point: Vector3): PointLocation.PointLocation = {
     val d = pseudoDistance(point)
     if(d < 0) {
       PointLocation.inside
     } else if (d > 0) {
       PointLocation.outside
     } else {
       PointLocation.neither 
     }
   }
}

object Plane {
  /**
   * Creates a plane based on 3 points
   */
   def toPlain(pointA: Vector3, pointB: Vector3,  pointC: Vector3): Plane = {
     val d1 = pointB - pointA
     val d2 = pointC - pointA
     val normal = (d1 crossProduct d2).normalize
     val constant = normal dot pointA
     Plane(normal, constant)
   }
}