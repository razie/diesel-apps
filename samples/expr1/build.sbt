scalaVersion := "2.11.12"

libraryDependencies in Global ++= Seq(
  "com.razie"              %% "diesel"         % "0.9.3-SNAPSHOT"
)

lazy val root = (project in file("."))

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
