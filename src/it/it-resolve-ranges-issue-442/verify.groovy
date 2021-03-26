def buildLogFile = new File(basedir, "build.log")
assert buildLogFile.text.contains("MojoExecutionException: Found invalid managed dependency junit:junit:jar without a version")