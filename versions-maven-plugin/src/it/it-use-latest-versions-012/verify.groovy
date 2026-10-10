def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<version>2.12.0.0</version>") : "Version of dummy-lib not bumped to 2.12.0.0"
