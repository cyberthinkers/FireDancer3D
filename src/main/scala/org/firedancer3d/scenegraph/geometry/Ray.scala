package org.firedancer3d.math
import Math._

case class Ray(origin: Vec3 = Vec3.zero, direction: Vec3 = Vec3.unitZ) {
  def distanceSquared(point: Vec3) = {
    val p1 = point - origin
    val t0 = direction dot p1
    val p2 = direction * t0 + origin
    val p3 = point - p2
    p3.lengthSquared
  }
  
  def distance(point: Vec3) = Math.sqrt(distanceSquared(point))
}