import sbt._
import Keys._
import play.PlayScala._
import play.sbt.routes.RoutesKeys

object V {
  val version      = "0.9.1-SNAPSHOT"
  val scalaVersion = "2.11.8" 
  val organization = "com.razie"

  def snap = (if (V.version endsWith "-SNAPSHOT") "-SNAPSHOT" else "")
}

object MyBuild extends Build {

  val appDependencies = Seq(
    "commons-codec"       % "commons-codec"      % "1.4",
    "javax.mail"          % "mail"               % "1.4.5",
    "ch.qos.logback"      % "logback-classic"    % "1.0.13",
    "org.mongodb"        %% "casbah"             % "2.8.2", //"2.6.5", //"2.5.0",
    "com.novus"          %% "salat-core"         % "1.9.9", //"1.9.2", //"1.9.6",
    "org.scalaz"         %% "scalaz-core"        % "7.2.1",
    "org.scalatest"      %% "scalatest"          % "2.1.3",
    "com.typesafe"        % "config"             % "1.2.1",
    "com.atlassian.commonmark"   % "commonmark"  % "0.5.0",

    "com.razie"          %% "base"               % ("0.9.1"),// +V.snap),
    "com.razie"          %% "snakked"            % ("0.9.1"),// +V.snap),
    "com.razie"          %% "scripster-core"     % ("0.9.1") // +V.snap)
    )

  lazy val root = Project("diesel", file(".")).enablePlugins(play.PlayScala).settings(
    defaultSettings:_*
                  ) aggregate (common, core) dependsOn (common, core)

  lazy val common = Project("diesel-common", file("common")).enablePlugins(play.PlayScala).settings(
    (defaultSettings ++ Seq(libraryDependencies ++= appDependencies)) :_*
                  )
  lazy val core = Project("diesel-core", file("core")).enablePlugins(play.PlayScala).settings(
    (defaultSettings ++ Seq(libraryDependencies ++= appDependencies)) :_*
                  ) dependsOn(common)

  def defaultSettings = baseSettings ++ Seq()

  def baseSettings = Defaults.defaultSettings ++ Seq (
    scalaVersion         := V.scalaVersion,
    version              := V.version,
    organization         := V.organization,
    organizationName     := "Razie's Pub",
    organizationHomepage := Some(url("http://www.razie.com")),

    publishTo <<= version { (v: String) =>
      if(v endsWith "-SNAPSHOT")
        Some ("Sonatype" at "https://oss.sonatype.org/content/repositories/snapshots/")
      else
        Some ("Sonatype" at "https://oss.sonatype.org/content/repositories/releases/")
    } ,

    resolvers ++= Seq("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
                      "releases"  at "https://oss.sonatype.org/content/repositories/releases") 
    )

}
