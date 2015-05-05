import sbt._
import Keys._
import play.Project._

object V {
  val version      = "0.1.3-SNAPSHOT"
  val scalaVersion = "2.10.3" 
  val organization = "com.razie"

  def snap = (if (V.version endsWith "-SNAPSHOT") "-SNAPSHOT" else "")
}

object ApplicationBuild extends Build {

  val appName         = "sample1"
  val appVersion      = V.version

  val appDependencies = Seq(
    cache,

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

  val repos = Seq("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "https://oss.sonatype.org/content/repositories/releases")

  val main = play.Project(appName, appVersion, appDependencies).settings(
      routesImport  += "model.Binders._",
      resolvers    ++= repos,
      scalaVersion := V.scalaVersion,
      unmanagedSourceDirectories in Compile += baseDirectory.value / "../../common",
      sources in doc in Compile := List()
    )

  override def rootProject = Some(main)
}

