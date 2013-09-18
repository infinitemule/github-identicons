
name := "github-identicons"

version := "1.0"

scalaVersion := "2.10.2"

resolvers += "Local Maven Repo" at "file://" + sys.env("MAVEN_LOCAL")

externalPom()