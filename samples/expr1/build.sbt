scalaVersion := "2.11.12"

libraryDependencies in Global ++= Seq(
  "com.razie"              %% "diesel"         % "0.9.4-SNAPSHOT"
)

lazy val root = (project in file("."))

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
