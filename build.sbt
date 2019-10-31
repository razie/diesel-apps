
scalaVersion := "2.11.12"

name := "diesel"

lazy val commonSettings = Seq(
  organization := "com.razie",
  version := "0.9.3-SNAPSHOT",
  scalaVersion := "2.11.12",

  organizationName     := "DieselApps",
  organizationHomepage := Some(url("http://www.dieselapps.com")),
  licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php")),
  homepage := Some(url("http://www.dieselapps.com"))
)

libraryDependencies in Global ++= Seq(
  cache,
  "org.json"                  % "json"             % "20160810",
  "commons-jxpath"            % "commons-jxpath"   % "1.3",
  "org.scala-lang.modules"   %% "scala-xml"        % "1.0.3",
  "ch.qos.logback"            % "logback-classic"  % "1.0.13",
  "commons-codec"             % "commons-codec"    % "1.4",
  "javax.mail"                % "mail"             % "1.4.5",
  "com.googlecode.java-diff-utils" % "diffutils"   % "1.2.1",
  "org.antlr"                 % "antlr4"           % "4.5.3",

  "org.mongodb"              %% "casbah"           % "2.8.2",
  "com.novus"                %% "salat-core"       % "1.9.9",

  "com.typesafe"              % "config"           % "1.2.1",

  "com.atlassian.commonmark"  % "commonmark"                % "0.9.0",
  "com.atlassian.commonmark"  % "commonmark-ext-gfm-tables" % "0.9.0",

  "com.typesafe.akka"        %% "akka-cluster"       % "2.4.2",
  "com.typesafe.akka"        %% "akka-cluster-tools" % "2.4.2",
  "com.typesafe.akka"        %% "akka-contrib"       % "2.4.2",
  "com.typesafe.akka"        %% "akka-slf4j"         % "2.4.2",
  "com.typesafe.akka"        %% "akka-camel"         % "2.4.2",
  "com.typesafe.akka"        %% "akka-testkit"       % "2.4.2" % Test,

  "junit"                   % "junit"              % "4.5"      % "test->default",
  "org.scalatest"          %% "scalatest"          % "2.1.3"

  //"com.razie"              %% "snakk_base"         % "0.9.3-SNAPSHOT",
  //"com.razie"              %% "snakk_core"         % "0.9.3-SNAPSHOT",
)

lazy val root = (project in file("."))
  .settings(
    commonSettings
  ).aggregate(diesel)//, pwiki)

// lazy val tconf = (project in file("tconf"))
//   .settings(
//     commonSettings,
//     unmanagedSourceDirectories in Compile += baseDirectory.value / "../../snakked/base/src/main/scala",
//     unmanagedSourceDirectories in Compile += baseDirectory.value / "../../snakked/core/src/main/scala"
//   )

lazy val diesel = (project in file("diesel"))
  .settings(
    commonSettings,
    unmanagedSourceDirectories in Compile += baseDirectory.value / "../../snakked/base/src/main/scala",
    unmanagedSourceDirectories in Compile += baseDirectory.value / "../../snakked/core/src/main/scala"
    // unmanagedSourceDirectories in Compile += baseDirectory.value / "../wiki/app"
  )

// lazy val diesel = (project in file("diesel"))
//   .settings(
//     commonSettings,
//     unmanagedSourceDirectories in Compile += baseDirectory.value / "../../snakked/base/src/main/scala",
//     unmanagedSourceDirectories in Compile += baseDirectory.value / "../../snakked/core/src/main/scala"
//   )
//   .dependsOn(tconf).aggregate(tconf)
//   .dependsOn(pcommon).aggregate(pcommon)

// lazy val pcommon = (project in file("common")).enablePlugins(PlayScala)
//   .settings(
//     commonSettings,
//     unmanagedSourceDirectories in Compile += baseDirectory.value / "../../snakked/base/src/main/scala",
//     unmanagedSourceDirectories in Compile += baseDirectory.value / "../../snakked/core/src/main/scala"
//   )
//   .dependsOn(diesel).aggregate(diesel)
//
// lazy val pwiki = (project in file("wiki")).enablePlugins(PlayScala)
//   .settings(
//     commonSettings,
//     unmanagedSourceDirectories in Compile += baseDirectory.value / "../../snakked/base/src/main/scala",
//     unmanagedSourceDirectories in Compile += baseDirectory.value / "../../snakked/core/src/main/scala"
//   )
//   .dependsOn(pcommon).aggregate(pcommon)


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
    <url>git@github.com:razie/diesel.git</url>
    <connection>scm:git:git@github.com:razie/diesel.git</connection>
  </scm>
  <developers>
    <developer>
      <id>razie</id>
      <name>Razvan Cojocaru</name>
      <url>http://www.dieselapps.com</url>
    </developer>
  </developers>
)
