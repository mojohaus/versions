def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<dummy.api.version>2.1</dummy.api.version>") : "Version of dummy-api not resolved"
