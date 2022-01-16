lazy val root = project
  .in(file("."))
  .settings(
    name := "network-access-test",
    description := "Utility for testing network accessibility of websites",
    version := "0.1.0",
    scalaVersion := "3.1.0",
    libraryDependencies ++= List(
        "commons-cli" % "commons-cli" % "1.5.0",
        "io.netty" % "netty-handler" % "4.1.73.Final",
        "org.typelevel" %% "cats-effect" % "3.3.4",
    )
)

assembly / assemblyMergeStrategy := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case _ => MergeStrategy.first
}

assembly / assemblyJarName := "network-access-test.jar"

