// https://github.com/rtimush/sbt-updates
// List libraries that are outdates via `dependancyUpdates`
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.4.2")

// Generic Native Packaging -- Used for Docker; Packaging only, no code changes
// [[https://github.com/sbt/sbt-native-packager]]
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.25")

//     https://github.com/sbt/sbt-bintray
// ~/.bintray/.credentials
// bintrayWhoami
addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.5")

// https://github.com/sbt/sbt-git
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")

// ---------- Code Coverage Goodies ---------------
// [[https://github.com/scoverage/sbt-scoverage]]
// sbt coverageAggregate to merge multi-module
//  https://github.com/scoverage/sbt-scoverage
// sbt clean coverage test
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.0")
