import sbt.Test
import sbtrelease.ReleaseStateTransformations._

val scala3Version    = "3.2.0"
val scalatestVersion = "3.2.14"
ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.sonatypeRepo("public"),
  Resolver.sonatypeRepo("snapshots"),
  "New snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots/"
)

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
    scalacOptions ++= Seq("-language:dynamics", "-Xcheck-macros"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    )
  ) ++ Publishing.publishSettings

lazy val bitlapx = project
  .in(file("."))
  .aggregate(`bitlapx-csv`, `bitlapx-common`, `bitlapx-json`)
  .settings(
    commands ++= Commands.value,
    crossScalaVersions            := Nil,
    publish / skip                := true,
    releaseIgnoreUntrackedFiles   := true,
    releaseCrossBuild             := false, // @see https://www.scala-sbt.org/1.x/docs/Cross-Build.html
    releaseTagName                := (ThisBuild / version).value,
    releasePublishArtifactsAction := PgpKeys.publishSigned.value,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      releaseStepCommandAndRemaining("+compile"),
      releaseStepCommandAndRemaining("test"),
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      releaseStepCommandAndRemaining("+publishSigned"),
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  )

lazy val `bitlapx-csv` = project
  .in(file("bitlapx-csv"))
  .settings(
    name := "bitlapx-csv",
    libraryDependencies ++= Seq(
      "com.softwaremill.magnolia1_3" %% "magnolia" % "1.1.1",
      "org.scalameta"                %% "munit"    % "1.0.0-M3" % Test
    )
  )
  .settings(commonSettings)
  .dependsOn(`bitlapx-common` % "compile->compile;test->test")

lazy val `bitlapx-json` = project
  .in(file("bitlapx-json"))
  .settings(
    name := "bitlapx-json",
    libraryDependencies ++= Seq(
      "com.softwaremill.magnolia1_3" %% "magnolia" % "1.1.1",
      "org.scalameta"                %% "munit"    % "1.0.0-M3" % Test
    )
  )
  .settings(commonSettings)
  .dependsOn(`bitlapx-common` % "compile->compile;test->test")

lazy val `bitlapx-common` = project
  .in(file("bitlapx-common"))
  .settings(
    name := "bitlapx-common"
  )
  .settings(commonSettings)
