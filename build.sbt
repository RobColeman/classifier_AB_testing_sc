name := "classifier_AB_testing_sc"

version := "1.0"

scalaVersion := "2.10.4"

resolvers += Resolver.sonatypeRepo("snapshots")

val scalaTestVersion = "2.2.4"
val breezeVersion = "0.11.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest"          % scalaTestVersion  % "test",
  "org.scalanlp"  %% "breeze"             % breezeVersion,
  "org.scalanlp"  %% "breeze-natives"     % breezeVersion,
  "org.apache.commons" % "commons-math3"  % "3.3",
  "gov.sandia.foundry" % "cognitive-foundry" % "3.3.2",
  "com.github.danielkorzekwa" %% "bayes-scala" % "0.5",
  "com.github.danielkorzekwa" %% "bayes-scala-gp" % "0.1-SNAPSHOT"
)