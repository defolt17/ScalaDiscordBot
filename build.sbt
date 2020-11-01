name := "ScalaDiscordBot"

version := "0.1"

scalaVersion := "2.13.3"

resolvers += Resolver.JCenterRepository

libraryDependencies += "net.katsstuff" %% "ackcord" % "0.17.1"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime