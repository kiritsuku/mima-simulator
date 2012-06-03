name := "mima-simulator"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.10" % "test",
  "junit" % "junit" % "4.7" % "test"
)

resolvers ++= Seq(
  "releases" at "http://oss.sonatype.org/content/repositories/releases"
)

publishTo := Some(Resolver.file("file", new File("bin/")))

EclipseKeys.withSource := true
