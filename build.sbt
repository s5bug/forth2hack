lazy val core = (project in file("core")).settings(
  organization := "tf.bug",
  name := "forth2hack",
  version := "0.1.0",
  scalaVersion := "2.13.5",
  idePackagePrefix := Some("tf.bug.forthhack"),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.6.0",
    "org.typelevel" %% "cats-effect" % "3.1.0",
    "com.monovore" %% "decline" % "2.0.0",
    "com.monovore" %% "decline-effect" % "2.0.0",
    "org.typelevel" %% "cats-parse" % "0.3.2",
    "co.fs2" %% "fs2-core" % "3.0.2",
    "co.fs2" %% "fs2-io" % "3.0.2",
    "org.typelevel" %% "cats-collections-core" % "0.9.2",
  ),
  mainClass := Some("tf.bug.forthhack.Main"),
).dependsOn(asmtrans)

lazy val asmtrans = (project in file("asmtrans")).settings(
  organization := "tf.bug",
  name := "hacktrans",
  version := "0.1.0",
  scalaVersion := "2.13.5",
  idePackagePrefix := Some("tf.bug.asmtrans"),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.6.0",
    "org.typelevel" %% "cats-effect" % "3.1.0",
    "com.monovore" %% "decline" % "2.0.0",
    "com.monovore" %% "decline-effect" % "2.0.0",
    "org.typelevel" %% "cats-parse" % "0.3.2",
    "co.fs2" %% "fs2-core" % "3.0.2",
    "co.fs2" %% "fs2-io" % "3.0.2",
    "org.typelevel" %% "cats-collections-core" % "0.9.2",
  ),
  mainClass := Some("tf.bug.asmtrans.Main"),
).enablePlugins(JavaAppPackaging)
