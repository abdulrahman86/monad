name := "monad"
organization := "sattar.scala"
version := "0.1-SNAPSHOT"
scalaVersion := "2.11.7"
libraryDependencies += "sattar.scala" %% "state" % "0.1-SNAPSHOT"
publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

