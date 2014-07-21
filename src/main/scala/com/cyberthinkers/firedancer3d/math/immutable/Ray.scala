package com.cyberthinkers.firedancer3d.math.immutable

case class Ray(origin: Vector3 = Vector3.zero, direction: Vector3 = Vector3.unitZ) {
  def distanceSquared(point: Vector3) = {
    val p1 = point - origin
    val t0 = direction dot p1
    val p2 = direction * t0 + origin
    val p3 = point - p2
    p3.lengthSquared
  }
}