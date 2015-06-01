import sbt._
import sbt.Keys._

import org.scalajs.sbtplugin._
//import ScalaJSPlugin.autoImport._
import org.scalajs.sbtplugin.cross._
//import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.scalajs.sbtplugin.cross.CrossProject
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.scalajs.sbtplugin.cross.CrossType

object FireDancer3DBuild extends Build {

lazy val globalSettings =
	Seq(organization := "firedancer3d.org", version := "0.1.0", scalaVersion := "2.11.6")
		
lazy val root =
	Project(id = "firedancer3d_root", base = file("."))
	.settings(globalSettings: _*)
	.aggregate(firedancer3d_shared, firedancer3d_jvm, firedancer3d_js)
	.dependsOn(firedancer3d_jvm, firedancer3d_js)

lazy val firedancer3d_shared =
	Project(id = "firedancer3d_shared", base = file("firedancer3d_shared"))
	.enablePlugins(ScalaJSPlugin)
	.settings(globalSettings: _*)

lazy val firedancer3d_jvm =
	Project(id = "firedancer3d_jvm", base = file("firedancer3d_jvm"))
	.settings(globalSettings: _*)
	.settings(unmanagedSourceDirectories in Compile += file("d:/cyberthinkers-dev/FireDancer3D/firedancer3d_shared") / "src")
	.settings(libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.5.1")
	.dependsOn(firedancer3d_shared)

lazy val firedancer3d_js =
	Project(id = "firedancer3d_js", base = file("firedancer3d_js"))
	.enablePlugins(ScalaJSPlugin)
	.settings(globalSettings: _*)
	//.settings(unmanagedSourceDirectories in Compile += file("d:/cyberthinkers-dev/FireDancer3D/firedancer3d_shared") / "src")
	.settings(libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0")
	.settings(libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.1")
	.dependsOn(firedancer3d_shared)
}