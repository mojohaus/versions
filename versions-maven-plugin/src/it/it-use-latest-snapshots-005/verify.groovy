def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<version>3.1.5-SNAPSHOT</version>") : "Version of dummy-api not bumped to 3.1.5-SNAPSHOT"
