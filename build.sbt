
scalaVersion := "2.11.8"

name := "diesel"

lazy val commonSettings = Seq(
  organization := "com.razie",
  version := "0.9.2-SNAPSHOT",
  scalaVersion := "2.11.8",

  organizationName     := "Razie's Pub",
  organizationHomepage := Some(url("http://www.razie.com")),
  licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php")),
  homepage := Some(url("http://www.razie.com"))
)

libraryDependencies in Global ++= Seq(
  "org.json"                % "json"               % "20160810",

  "commons-jxpath"          % "commons-jxpath"     % "1.3",
  "org.scala-lang.modules" %% "scala-xml"          % "1.0.3",

  "ch.qos.logback"          % "logback-classic"    % "1.0.13",

  //"com.razie"              %% "snakk_base"         % "0.9.2-SNAPSHOT",
  //"com.razie"              %% "snakk_core"         % "0.9.2-SNAPSHOT",

  "org.mongodb"            %% "casbah"             % "2.8.2",
  "com.novus"              %% "salat-core"         % "1.9.9",

  "junit"                   % "junit"              % "4.5"      % "test->default",
  "org.scalatest"          %% "scalatest"          % "2.1.3"
)

lazy val root = (project in file("."))
  .settings(
    commonSettings
  ).aggregate(diesel, pcommon)

lazy val diesel = (project in file("diesel"))
  .settings(
    commonSettings,
 // libraryDependencies ++= deps
    unmanagedSourceDirectories in Compile += baseDirectory.value / "../../snakked/base/src/main/scala",
    unmanagedSourceDirectories in Compile += baseDirectory.value / "../../snakked/core/src/main/scala"
  )

lazy val pcommon = (project in file("common")).enablePlugins(PlayScala)
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      cache,
      "commons-codec"         % "commons-codec"      % "1.4",
      "javax.mail"            % "mail"               % "1.4.5",

      "com.atlassian.commonmark"   % "commonmark"  % "0.7.0",
      "com.typesafe"          % "config"             % "1.2.1",

      "com.googlecode.java-diff-utils"        % "diffutils"             % "1.2.1",

      "org.antlr" % "antlr4" % "4.5.3"
   ),
    unmanagedSourceDirectories in Compile += baseDirectory.value / "../../snakked/base/src/main/scala",
    unmanagedSourceDirectories in Compile += baseDirectory.value / "../../snakked/core/src/main/scala"
  )
  .dependsOn(diesel).aggregate(diesel)

/*lazy val wiki = (project in file("wiki"))
  .settings(
    commonSettings  //, libraryDependencies ++= deps
  )
  .dependsOn(diesel,common) */

retrieveManaged := true // copy libs in lib_managed

/* resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "https://oss.sonatype.org/content/repositories/public"
) */

resolvers += 
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

publishTo in Global := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

pomExtra := (
  <url>http://www.razie.com</url>
  <licenses>
    <license>
      <name>BSD-style</name>
      <url>http://www.opensource.org/licenses/bsd-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:razie/diesel-hydra.git</url>
    <connection>scm:git:git@github.com:razie/diesel-hydra.git</connection>
  </scm>
  <developers>
    <developer>
      <id>razie</id>
      <name>Razvan Cojocaru</name>
      <url>http://www.dieselapps.com</url>
    </developer>
  </developers>
)
