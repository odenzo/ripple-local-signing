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

// --------------- Documentation Goodies --------------

//addSbtPlugin("com.github.xuwei-k" % "sbt-class-diagram" % "0.2.1")

//
// ----------- Publishing ---------------
//
//addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.4")
//addSbtPlugin("com.jsuereth"   % "sbt-pgp"      % "1.1.0")

//     https://github.com/sbt/sbt-bintray
// ~/.bintray/.credentials
// bintrayWhoami
//addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.4")

// ---------- Code Coverage Goodies ---------------
// [[https://github.com/scoverage/sbt-scoverage]]
// sbt coverageAggregate to merge multi-module
//  https://github.com/scoverage/sbt-scoverage
// sbt clean coverage test
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.0")

// Open Source
// https://github.com/scoverage/sbt-coveralls
// https://coveralls.io/
//addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.1.0")
// [[https://github.com/codacy/sbt-codacy-coverage]] Post test coverage to codacity
// sbt coverageAggregate after (or before) runnign all tests
// sbt clean coverage test
//sbt coverageReport
//sbt coverageAggregate
//sbt codacyCoverage
//     export CODACY_PROJECT_TOKEN=%Project_Token%
//addSbtPlugin("com.codacy" % "sbt-codacy-coverage" % "1.3.8")

/*
If you have a multi-module project, perform coverageAggregate as a separate command

script:
  - sbt clean coverage test coverageReport &&
    sbt coverageAggregate
after_success:
  - sbt coveralls
 */


//Cross-building in addition to SBT Cross-Project Stuff - this isn't for Scala version x-project
// https://github.com/portable-scala/sbt-crossproject
//addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "0.6.1")
//addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "0.6.1")
//addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "0.6.23")
//addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.3.7")
