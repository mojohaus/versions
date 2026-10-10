def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<api>2.0</api>") : "Version of api not resolved to 2.0"
