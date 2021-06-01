import sbt.Keys.scalacOptions

val scala3Version = "3.0.0"

lazy val typesafeConfigVersion = "1.4.1"

scalaVersion in Scope.Global := scala3Version

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-simple",
    version := "0.1.0"
  ).aggregate(containerMacro, container)


// lazy val `macro` = (project in file("refuel-container-macro"))
//   .settings(
//     name := "refuel-container-macro",
//     description := "Lightweight DI container for Scala.",
//     libraryDependencies ++= {
//       Seq(
//         "com.typesafe"   % "config"        % typesafeConfigVersion
//       )
//     },
//     scalacOptions += "-language:experimental.macros"
//   )

lazy val containerMacro = (project in file("refuel-container-macro"))
  .settings(
    name := "refuel-container-macro",
    description := "Lightweight DI container for Scala.",
    libraryDependencies ++= {
      Seq(
        "com.typesafe"   % "config"        % typesafeConfigVersion,
        "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
        "org.scala-lang" %% "scala3-staging" % scalaVersion.value
      )
    },
    scalacOptions ++= Seq(
      //            "-Ydebug",
      //             "-Ymacro-debug-verbose",
    )
  )


lazy val container = (project in file("refuel-container"))
  .dependsOn(containerMacro)
  .settings(
    name := "refuel-conatiner",
    description := "Lightweight DI container for Scala.",
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-staging" % scalaVersion.value,
      "org.scalatest" %% "scalatest" % "3.2.9" % Test
    )
  )
