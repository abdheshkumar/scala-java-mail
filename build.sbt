val catsV = "2.0.0"
val catsEffectV = "2.0.0"
val javaxMailV = "1.6.2"
val scalaLoggingV = "3.9.2"
val pureConfigV = "0.13.0"
lazy val root = (project in file(".")).settings(
  name := "scala-java-mail",
  version := "0.1",
  scalaVersion := "2.12.7",
  libraryDependencies := Seq(
    "org.typelevel" %% "cats-core" % catsV,
    "org.typelevel" %% "cats-effect" % catsEffectV,
    "com.sun.mail" % "javax.mail" % javaxMailV,
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingV,
    "com.github.pureconfig" %% "pureconfig" % pureConfigV
  )
)
