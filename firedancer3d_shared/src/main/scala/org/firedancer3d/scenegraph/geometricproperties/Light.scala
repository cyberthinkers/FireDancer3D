package org.firedancer3d.scenegraph.geometricproperties
import org.firedancer3d.math._

abstract class Light

case class DirectionalLight(
    direction: Vec3 = Vec3(1, 1, 1),
    color: ColorRGB = ColorRGB(1, 1, 1),
    intensity: Float = 1,
    ambientIntensity: Float = 0) extends Light {
}

case class PointLight(
    direction: Vec3 = Vec3(1, 1, 1),
    location: Vec3 = Vec3(0, 0, 0),
    color: ColorRGB = ColorRGB(1, 1, 1),
    intensity: Float = 1,
    ambientIntensity: Float = 0,
    radius: Float = 100) extends Light {
}

case class SpotLight(
    direction: Vec3 = Vec3(0, 0, -1),
    location: Vec3 = Vec3(0, 0, 0), 
    color: ColorRGB = ColorRGB(1, 1, 1),
    intensity: Float = 1,
    ambientIntensity: Float = 0,
    radius: Float = 100,
    beamWidth: Double = 1.570796,
    cutOffAngle: Double = 0.785398) extends Light {
}