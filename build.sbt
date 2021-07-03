val scalatestVersion = "3.2.9"

lazy val root = (project in file(".")).settings(
  inThisBuild(List(
    organization := "net.imadz",
    scalaVersion := "2.12.13"
  )),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % scalatestVersion % Test
  ),
  name := "whyfp90"
)
