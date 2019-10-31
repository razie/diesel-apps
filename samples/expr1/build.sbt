
scalaVersion := "2.11.12"

name := "expr1"

lazy val commonSettings = Seq(
  version := "0.9.3-SNAPSHOT"
)

libraryDependencies in Global ++= Seq(
  "com.razie"              %% "diesel"         % "0.9.3-SNAPSHOT"
)

lazy val root = (project in file("."))
  .settings(
    commonSettings
  )

retrieveManaged := true // copy libs in lib_managed

resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
