def buildLogFile = new File(basedir, "build.log")
assert buildLogFile.text.contains("Found managed dependency localhost:dummy-test-lib:jar without a version")
