name := "swing-diode-experiment"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies += "io.suzaku" %% "diode" % "1.1.3"
libraryDependencies += "com.miglayout" % "miglayout" % "3.7.4"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")

enablePlugins(JavaAppPackaging)
enablePlugins(JDKPackagerPlugin)

jdkPackagerType := "image"