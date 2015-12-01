scalaSource in Compile := baseDirectory.value / "src"

unmanagedBase <<= baseDirectory { base => base / "lib" }

scalaVersion := "2.11.7"

javaOptions += "-Xmx6G"
