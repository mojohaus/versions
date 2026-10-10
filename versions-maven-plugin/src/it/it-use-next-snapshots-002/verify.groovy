def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<version>1.1.2-SNAPSHOT</version>") : "Version of dummy-api bumped"
