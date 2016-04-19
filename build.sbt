version in ThisBuild := "2.0.0-SNAPSHOT"

val shimsVersion = "0.3"

val commonSettings = Seq (
  resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
  libraryDependencies ++= Seq(
    "com.codecommit" %% "shims-core" % shimsVersion,
    "com.typesafe.play" %% "play-specs2" % "2.4.3" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"
  )
)

lazy val core = (project in file("core")).enablePlugins(PlayScala)
  .settings(commonSettings:_*)
  .settings(name := "play-monadic-actions")

lazy val scalaz = (project in file("scalaz"))
  .settings(commonSettings:_*)
  .settings(
    name := "play-monadic-actions-scalaz",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.1.0",
      "com.codecommit" %% "shims-scalaz-71" % shimsVersion
    )
  )
  .dependsOn(core)
  .enablePlugins(PlayScala)


lazy val cats = (project in file("cats"))
  .settings(commonSettings:_*)
  .settings(
    name := "play-monadic-actions-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats" % "0.4.1",
      "com.codecommit" %% "shims-cats" % shimsVersion
    )
  )
  .dependsOn(core)
  .enablePlugins(PlayScala)


scalaVersion in ThisBuild := "2.11.8"


organization in ThisBuild := "io.kanaka"

description := "Mini DSL to allow the writing of Play! actions using for-comprehensions"

publishMavenStyle in ThisBuild := true

licenses in ThisBuild += ("Apache2", url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage in ThisBuild := Some(url("https://github.com/Kanaka-io/play-monadic-actions"))

pomExtra in ThisBuild := <scm>
  <url>git@github.com:Kanaka-io/play-monadic-actions.git</url>
  <connection>scm:git:git@github.com:Kanaka-io/play-monadic-actions.git</connection>
</scm>
  <developers>
    <developer>
      <id>vkasas</id>
      <name>Valentin Kasas</name>
      <url>https://twitter.com/ValentinKasas</url>
    </developer>
  </developers>

publishArtifact in Test := false

publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}
