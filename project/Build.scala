import sbt._
import sbt.Keys._

object ProjectBuild extends Build {

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "la",
      organization := "com.github.la",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.9.1",
      scalacOptions ++= Seq("-unchecked", "-deprecation"),
      libraryDependencies += "org.specs2" %% "specs2" % "1.8.2" % "test",
      resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                    "releases"  at "http://oss.sonatype.org/content/repositories/releases")
    )
  )
}
