import sbt.Test

val scala3Version    = "3.2.2"
val scalatestVersion = "3.2.14"

Global / onChangedBuildSource := ReloadOnSourceChanges
lazy val commonSettings =
  Seq(
    organization                  := "org.bitlap",
    startYear                     := Some(2023),
    scalaVersion                  := scala3Version,
    Compile / compile             := (Compile / compile).dependsOn(Compile / headerCreateAll).value,
    Global / onChangedBuildSource := ReloadOnSourceChanges,
    headerLicense                 := Some(HeaderLicense.MIT("2023", "bitlap")),
    Test / testOptions += Tests.Argument("-oDF"),
    Test / fork               := true,
    publishConfiguration      := publishConfiguration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
    scalacOptions ++= Seq(
      /** "-Ycheck:all",** */
      "-language:dynamics",
      "-explain",
      "-release:8",
      "unchecked",
      "-deprecation",
      "-feature",
      "-Werror"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    )
  )

lazy val `scala3-metaprogramming-examples` = project
  .in(file("."))
  .aggregate(`csv`, `common`, `json`)
  .settings(
    commands ++= Commands.value,
    crossScalaVersions := Nil,
    publish / skip     := true
  )

lazy val `csv` = project
  .in(file("csv"))
  .settings(
    name := "csv",
    libraryDependencies ++= Seq(
      "com.softwaremill.magnolia1_3" %% "magnolia" % "1.1.1",
      "org.scalameta"                %% "munit"    % "1.0.0-M3" % Test
    )
  )
  .settings(commonSettings)
  .dependsOn(`common` % "compile->compile;test->test")

lazy val `json` = project
  .in(file("json"))
  .settings(
    name := "json",
    libraryDependencies ++= Seq(
      "com.softwaremill.magnolia1_3" %% "magnolia" % "1.1.1",
      "org.scalameta"                %% "munit"    % "1.0.0-M3" % Test
    )
  )
  .settings(commonSettings)
  .dependsOn(`common` % "compile->compile;test->test")

lazy val `common` = project
  .in(file("common"))
  .settings(
    name := "common",
    libraryDependencies ++= Seq(
      "com.softwaremill.magnolia1_3" %% "magnolia" % "1.1.1",
      "org.scalameta"                %% "munit"    % "1.0.0-M3" % Test
    )
  )
  .settings(commonSettings)

coverageHighlighting            := true
coverageFailOnMinimum           := false
coverageMinimumStmtTotal        := 70
coverageMinimumBranchTotal      := 70
coverageMinimumStmtPerPackage   := 70
coverageMinimumBranchPerPackage := 70
coverageMinimumStmtPerFile      := 70
coverageMinimumBranchPerFile    := 70
