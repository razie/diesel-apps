import sbt._
import Keys._

object V {
  val version      = "0.1.6-SNAPSHOT"
  val scalaVersion = "2.10.4" 
  val organization = "com.razie"

  def snap = (if (V.version endsWith "-SNAPSHOT") "-SNAPSHOT" else "")

  def RAZBASEVER = "0.6.6" + snap
}

object MyBuild extends Build {

  val appDependencies = Seq(
    "commons-codec"       % "commons-codec"      % "1.4",
    "javax.mail"          % "mail"               % "1.4.5",
    "ch.qos.logback"      % "logback-classic"    % "1.0.13",
    "org.mongodb"        %% "casbah"             % "2.6.5", //"2.5.0",
    "com.novus"          %% "salat-core"         % "1.9.2", //"1.9.6",
    "com.tristanhunt"    %% "knockoff"           % "0.8.1",
    "org.scalaz"         %% "scalaz-core"        % "7.0.3",
    "org.scalatest"      %% "scalatest"          % "1.9.2",
    "com.typesafe"        % "config"             % "1.2.1",

    "com.razie"          %% "base"               % "0.6.6-SNAPSHOT",
    "com.razie"          %% "snakked"            % "0.6.6-SNAPSHOT",
    "com.razie"          %% "scripster-core"     % "0.8.6-SNAPSHOT"
    )

  lazy val root = Project(id="coolscala",    base=file("."),
                          settings = defaultSettings ++ Seq()
                  ) aggregate (common, core) dependsOn (common, core)

  lazy val common = Project(id="coolscala-common", base=file("common"),
                          settings = defaultSettings ++ 
                          Seq(libraryDependencies ++= appDependencies)
                  )
  lazy val core = Project(id="coolscala-core", base=file("core"),
                          settings = defaultSettings ++ 
                          Seq(libraryDependencies ++= appDependencies)
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
