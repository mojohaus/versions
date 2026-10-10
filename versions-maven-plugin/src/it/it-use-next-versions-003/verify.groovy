def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<version>1.1.2</version>") : "Version of dummy-api was not bumped"
