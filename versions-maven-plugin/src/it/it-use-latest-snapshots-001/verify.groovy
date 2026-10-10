def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<version>1.9.1-SNAPSHOT</version>") : "Version of dummy-api not bumped to 1.9.1-SNAPSHOT"
