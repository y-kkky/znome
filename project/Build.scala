import sbt._
import Keys._
import play.Project._
import org.ensime.sbt.Plugin.Settings.ensimeConfig
import org.ensime.sbt.util.SExp._

object ApplicationBuild extends Build {

  val appName         = "zno"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    cache,
    filters,
    "com.typesafe.slick" %% "slick" % "2.0.0",
    "org.slf4j" % "slf4j-nop" % "1.6.4",
    "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
    "com.typesafe" %% "play-plugins-mailer" % "2.1.0"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here
    lessEntryPoints <<= baseDirectory(_ / "app" / "assets" / "stylesheets" ** "main.less"),
      // Add your own project settings here
    ensimeConfig := sexp(
      key(":only-include-in-index"), sexp(
        "controllers\\..*",
        "models\\..*",
        "views\\..*"
      )
    )
  )

}
