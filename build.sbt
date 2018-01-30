lazy val commonSettings = Seq(
  organization := "hmrclt",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.12.4",
  libraryDependencies += "io.frees" %% "frees-core" % "0.5.2",
  scalacOptions ++= Seq("-feature")
)

lazy val common = (project in file("."))
  .settings(commonSettings)
  .settings(
    addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full)
  )

lazy val gui = project
  .settings(commonSettings)
  .settings(
     libraryDependencies += "org.typelevel" %% "cats-effect" % "0.8"
  )
  .dependsOn(common)

lazy val logictable = project
  .settings(commonSettings)
  .dependsOn(common)

lazy val webfe = project
  .enablePlugins(PlayScala)
  .settings(commonSettings)
  .settings(
    libraryDependencies += guice,
  ).dependsOn(common)

lazy val selenium = project
  .settings(commonSettings)
  .settings(
    libraryDependencies += "org.seleniumhq.selenium" % "selenium-firefox-driver" % "3.8.1",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "0.8"    
  )
  .dependsOn(common)
