def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<api>2.1.1-SNAPSHOT</api>") : "Version of api not updated to 2.1.1-SNAPSHOT"
