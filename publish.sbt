//
//bintrayRepository := (if (isSnapshot.value) "sbt-plugin-snapshots" else "sbt-plugins")
//
//bintrayOrganization in bintray := None
//
//bintrayReleaseOnPublish := isSnapshot.value
enablePlugins(JavaAppPackaging)

licenses := List(("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0")))
homepage := Some(url("https://github.com/odenzo/ripple-local-signing"))

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

publishMavenStyle                    := true
publishArtifact in Test              := true
publishMavenStyle                    := true
bintrayOrganization                  := Some("odenzooss")
bintrayPackage                       := "ripple-local-signing"
bintrayReleaseOnPublish in ThisBuild := false
