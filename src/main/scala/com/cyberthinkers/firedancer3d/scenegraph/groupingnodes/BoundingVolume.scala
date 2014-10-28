package com.cyberthinkers.firedancer3d.math

import Math._

abstract class BoundingVolume {
   def contains(point: Vector3): Boolean
   def distanceSquared(point: Vector3): Double
   def distance(point: Vector3): Double
}

/**
 * Axis-aligned bounding box (AABB).
 * 
 * This is a simple six-sided box with each side parallel to one of the cardinal planes. 
 * The box is not necessarily a cube-the length, width, and height of the box may each be different.
 * (See "3D Math Primer for Graphics and Game Development," by Dunn and Parberry)
 */
case class AxisAlignedBoundingBox(minExtents: Vector3, maxExtents: Vector3) extends BoundingVolume {

  def this(box: AxisAlignedBoundingBox) = this(box.minExtents, box.maxExtents)
  
  def this(cubeSize: Double) = this(AxisAlignedBoundingBox.toAxisAlignedBoundingBox(cubeSize))
  
  def corner(index: Integer): Vector3 = {
    require(index >= 0 && index < 7)
    Vector3(
      if((index & 1) != 0) maxExtents.x else minExtents.x,
      if((index & 2) != 0) maxExtents.y else minExtents.y,
      if((index & 4) != 0) maxExtents.z else minExtents.z)
  }
  
  def +(point: Vector3): AxisAlignedBoundingBox = {
    val min = Vector3(if(point.x < minExtents.x) point.x else minExtents.x,
                      if(point.y < minExtents.y) point.y else minExtents.y,
                      if(point.z < minExtents.z) point.z else minExtents.z)
    val max = Vector3(if(point.x > maxExtents.x) point.x else maxExtents.x,
                      if(point.y > maxExtents.y) point.y else maxExtents.y,
                      if(point.z > maxExtents.z) point.z else maxExtents.z)
    AxisAlignedBoundingBox(min, max)
  }
  
  def +(box: AxisAlignedBoundingBox) = {
    val min = Vector3(if(box.minExtents.x < minExtents.x) box.minExtents.x else minExtents.x,
                      if(box.minExtents.y < minExtents.y) box.minExtents.y else minExtents.y,
                      if(box.minExtents.z < minExtents.z) box.minExtents.z else minExtents.z)
    val max = Vector3(if(box.maxExtents.x > maxExtents.x) box.maxExtents.x else maxExtents.x,
                      if(box.maxExtents.y > maxExtents.y) box.maxExtents.y else maxExtents.y,
                      if(box.maxExtents.z > maxExtents.z) box.maxExtents.z else maxExtents.z)
    AxisAlignedBoundingBox(min, max)
  }
  
  def *(scale: Vector3): AxisAlignedBoundingBox = AxisAlignedBoundingBox(minExtents * scale, maxExtents * scale)
  
  def *(scale: Double): AxisAlignedBoundingBox = AxisAlignedBoundingBox(minExtents * scale, maxExtents * scale)
  
  def ==~(that: AxisAlignedBoundingBox): Boolean = (this.minExtents ==~ that.minExtents) && (this.maxExtents ==~ that.maxExtents)
  
  def distanceSquared(point: Vector3): Double = {
    def squared(d: Double) = d * d
    def dist(p: Double, min: Double, max: Double) = if(p < min) squared(min - p) else squared(p - max) 
    dist(point.x, minExtents.x, maxExtents.x) +
    dist(point.y, minExtents.y, maxExtents.y) +
    dist(point.z, minExtents.z, maxExtents.z)
  }
  
  def distance(point: Vector3): Double = Math.sqrt(distanceSquared(point))
  
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
  
  def intersect(that: AxisAlignedBoundingBox) = {
    AxisAlignedBoundingBox(
      Vector3.min(this.minExtents, that.minExtents),
      Vector3.min(this.maxExtents, that.maxExtents))
  }
  
  def contains(box: AxisAlignedBoundingBox): Boolean = {
    minExtents.x <= box.minExtents.x &&
    minExtents.y <= box.minExtents.y &&
    minExtents.z <= box.minExtents.z &&
    maxExtents.x >= box.maxExtents.x &&
    maxExtents.y >= box.maxExtents.y &&
    maxExtents.z >= box.maxExtents.z
  }
  
  def overlaps(box: AxisAlignedBoundingBox): Boolean = {
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

object AxisAlignedBoundingBox {
  
  val emptyBox = AxisAlignedBoundingBox(
    Vector3(Double.MaxValue, Double.MaxValue, Double.MaxValue),
    Vector3(Double.MinValue, Double.MinValue, Double.MinValue))
    
  def toAxisAlignedBoundingBox(cubeSize: Double): AxisAlignedBoundingBox =
    {val v = cubeSize * .5; AxisAlignedBoundingBox(Vector3(-v, -v, -v), Vector3(v, v, v))}
}

//case class OrientedBoundingBox() extends BoundingVolume {
//
//}

case class BoundingSphere(center: Vector3, radius: Double) extends BoundingVolume {
  
  def contains(point: Vector3): Boolean = (center - point).lengthSquared <= radius * radius
  
  def contains(that: BoundingSphere): Boolean = {
    if(this.radius < that.radius) {
      false
    }
    val r = this.radius - that.radius
    (this.center - that.center).lengthSquared <= r * r
  }

  def distanceSquared(point: Vector3): Double = (center - point).lengthSquared - radius * radius
  
  def distance(point: Vector3): Double = (center - point).length - radius
}