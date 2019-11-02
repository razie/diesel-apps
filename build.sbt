
scalaVersion := "2.11.12"

name := "diesel"

publishMavenStyle := true

lazy val commonSettings = Seq(
  scalaVersion := "2.11.12",
  organization := "com.razie",
  version := "0.9.4-SNAPSHOT",
  // in sbt 1: publishConfiguration := publishConfiguration.value.withOverwrite(true),
  organizationName := "DieselApps",

  publishMavenStyle := true,

  organizationHomepage := Some(url("http://www.dieselapps.com")),

  homepage := Some(url("http://www.dieselapps.com")),

  scmInfo := Some(ScmInfo(url("https://github.com/razie/diesel"),
                        "git@github.com:razie/diesel.git")),

  developers := List(Developer("razie",
                             "Razvan Cojocaru",
                             "r@razie.com",
                             url("https://github.com/razie"))),

  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

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

publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(Path.userHome / ".ivy2.credentials")

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

