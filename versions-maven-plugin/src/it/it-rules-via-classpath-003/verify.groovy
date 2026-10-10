def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<version>3.0.1-1.1</version>") : "Version of version-rules not bumped to 3.0.1-1.1"
