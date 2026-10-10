def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<maven.version>2.0.10</maven.version>") : "maven.version should not have changed (version conflict)"
