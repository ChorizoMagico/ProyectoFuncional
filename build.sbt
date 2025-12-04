ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "ProyectoPFC",
    fork := true, // Necesario para la memoria

    // Agregamos este resolver por seguridad, ya que ScalaMeter a veces se esconde
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",

    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",

      // CAMBIO CLAVE: "com.storm-enroute" en lugar de "org.scalameter"
      "com.storm-enroute" %% "scalameter" % "0.19"
    ),

    javaOptions ++= Seq(
      "-Xmx8G",
      "-XX:-UseGCOverheadLimit",
      "-Xms1G",
      "-XX:+UseG1GC"
    )
  )