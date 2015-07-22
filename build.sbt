import play.PlayScala

name := """just-play-scala"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.4"

lazy val root = project.in(file(".")).enablePlugins(PlayScala)


fork in run := true

