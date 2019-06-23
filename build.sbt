import MyCompileOptions._
import sbt.Keys.resolvers

ThisBuild / organization := "com.odenzo"
ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.0.2"

name := "rippled-signing"

scalacOptions ++= Seq("-feature",
                      "-deprecation",
                      "-unchecked",
                      "-language:postfixOps",
                      "-language:higherKinds",
                      "-Ypartial-unification")

lazy val signing = (project in file("."))
  .settings(
    commonSettings,
    devSettings,
    scalacOptions ++= opts ++ warnings ++ linters
  )

resolvers += Resolver.bintrayRepo("odenzooss", "maven")
resolvers += Resolver.jcenterRepo // Will be moving there soon
libraryDependencies += "com.odenzo" %% "ripple-binary-codec" % "0.2.2"

lazy val commonSettings = Seq(
  libraryDependencies ++= libs ++ lib_circe ++ lib_cats ++ lib_spire ++ lib_bouncycastle ++ lib_scribe,
  resolvers ++= Seq(
    //Resolver.defaultLocal, // Usual I pulishLocal to Ivy not maven
    Resolver.jcenterRepo, // This is JFrogs Maven Repository for reading
    Resolver.bintrayRepo("odenzooss", "maven")
  )
)
val devSettings = Seq(
  Test / logBuffered := false,
  Test / parallelExecution := false,
)


val libs = {
  Seq(
    "org.scalatest"              %% "scalatest"      % "3.0.8" % Test,
    "org.scalacheck"             %% "scalacheck"     % "1.14.0" % Test,
  )
}

/** JSON Libs == Circe and Associated Support Libs */
val lib_circe = {
  val circeVersion = "0.11.1"

  Seq(
    "io.circe" %% "circe-core"           % circeVersion,
    "io.circe" %% "circe-generic"        % circeVersion,
    "io.circe" %% "circe-java8"          % circeVersion,
    "io.circe" %% "circe-parser"         % circeVersion,
    "io.circe" %% "circe-generic-extras" % circeVersion,
    )

}

val lib_cats = {
  val catsVersion = "1.6.1"
  Seq(
    "org.typelevel" %% "cats-core"   % catsVersion, // Cats is pulled in via Circe for now
    "org.typelevel" %% "cats-effect" % "1.3.1" withSources () withJavadoc ()
  )
}

val lib_spire = {
  Seq(
    "org.typelevel" %% "spire" % "0.16.2"
  )
}



val lib_scribe = {
  Seq("com.outr" %% "scribe" % "2.7.7")
}


val lib_bouncycastle = {
  val version = "1.62"
  Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % version
  )
}
