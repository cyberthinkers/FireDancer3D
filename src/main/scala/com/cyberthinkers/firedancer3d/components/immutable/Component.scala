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

case class Box3(minExtents: Vector3, maxExtents: Vector3) extends Component {
  
  def this(cubeSize: Double) = this(Box3.toBox(-cubeSize), Box3.toBox(cubeSize))
  
  def *(p: Vector3): Box3 = {Box3(minExtents * p, maxExtents * p)}
  
  def *(scale: Double): Box3 = {Box3(minExtents * scale, maxExtents * scale)}
  
  def ~=(box: Box3) = {
    (minExtents ~= box.minExtents) && (maxExtents ~= box.maxExtents)
  }
  
  def center: Vector3 = {
    Vector3(  
      (minExtents.x + maxExtents.x) * .5,
      (minExtents.y + maxExtents.y) * .5,
      (minExtents.z + maxExtents.z) * .5)
  }
  
  def volume: Double = {
    (maxExtents.x - minExtents.x) *
    (maxExtents.y - minExtents.y) *
    (maxExtents.z - minExtents.z)
  }
  
  def contains(point: Vector3): Boolean = {
    point.x >= minExtents.x && point.x < maxExtents.x &&
    point.y >= minExtents.y && point.y < maxExtents.y &&
    point.z >= minExtents.z && point.z < maxExtents.z
  }
  
  def intersect(that: Box3) = {
    Box3(
      Vector3.min(this.minExtents, that.minExtents),
      Vector3.min(this.maxExtents, that.maxExtents))
  }
  
  def contains(box: Box3): Boolean = {
    minExtents.x <= box.minExtents.x &&
    minExtents.y <= box.minExtents.y &&
    minExtents.z <= box.minExtents.z &&
    maxExtents.x >= box.maxExtents.x &&
    maxExtents.y >= box.maxExtents.y &&
    maxExtents.z >= box.maxExtents.z
  }
  
  def overlaps(box: Box3): Boolean = {
    if(box.minExtents.x > maxExtents.x ||
       box.minExtents.y > maxExtents.y ||
       box.minExtents.z > maxExtents.z ||
       box.maxExtents.x < minExtents.x ||
       box.maxExtents.y < minExtents.y ||
       box.maxExtents.z < minExtents.z) {
       false
    }
    true
  }
}

object Box3 {
  def toBox(cubeSize: Double): Vector3 = {val v = cubeSize * .5; Vector3(v, v, v)}
}

object PointLocation extends Enumeration {
  type PointLocation = Value
  val front, on, back = Value
}

case class Plane(normal: Vector3 = Vector3.unitY, constant: Double = 0) extends Component {
  
   def this(plane: Plane) = this(plane.normal, plane.constant)
   
   def this(pointA: Vector3, pointB: Vector3, pointC: Vector3) = this(Plane.toPlane(pointA, pointB, pointC))
   
   def pseudoDistance(point: Vector3): Double = (normal dot point) - constant
   
   /**
    * Determines the side of this plane on which the given point lies
    */
   def pointLocation(point: Vector3): PointLocation.PointLocation = {
     val d = pseudoDistance(point)
     if(d <= 0.005) {
       PointLocation.front
     } else if (d >= -0.005) {
       PointLocation.back
     } else {
       PointLocation.on 
     }
   }
}

object Plane {
  /**
   * Creates a plane from 3 points
   */
   def toPlane(point1: Vector3, point2: Vector3,  point3: Vector3): Plane = {
     val d1 = point2 - point1
     val d2 = point3 - point1
     val normal = (d1 crossProduct d2).normalize
     val constant = normal dot point1
     Plane(normal, constant)
   }
}