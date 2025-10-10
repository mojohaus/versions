def buildLogFile = new File(basedir, "build.log")
assert buildLogFile.text.contains("Found managed dependency junit:junit:jar without a version")
