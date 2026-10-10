def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<version>3.0</version>") : "Version of dummy-api not bumped to 3.0"
