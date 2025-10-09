def buildLogFile = new File(basedir, "build.log")
assert buildLogFile.text.contains("Found managed dependency test.package:moduleTest:jar without a version")
