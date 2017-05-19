name := """diesel-common"""

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.8"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

//unmanagedSourceDirectories in Compile += baseDirectory.value / "../snakked/core/src/main/scala"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test,

    "commons-codec"         % "commons-codec"      % "1.4",
    "javax.mail"            % "mail"               % "1.4.5",
    "ch.qos.logback"        % "logback-classic"    % "1.0.13",
    "org.mongodb"          %% "casbah"             % "2.8.2",
    "com.novus"            %% "salat-core"         % "1.9.9",
    "com.atlassian.commonmark"   % "commonmark"  % "0.7.0",
    "com.typesafe"          % "config"             % "1.2.1",

    "com.googlecode.java-diff-utils"        % "diffutils"             % "1.2.1",

     "com.razie"            %% "base"               % "0.9.1",//-SNAPSHOT",
     "com.razie"            %% "snakked"            % "0.9.1",//-SNAPSHOT",
    // "com.razie"            %% "scripster-core"     % "0.9.2"//-SNAPSHOT"

    "org.antlr" % "antlr4" % "4.5.3"
)
