def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<api>2.1.1-SNAPSHOT</api>") : "Version of api not updated to 2.1.1-SNAPSHOT"
assert buf.contains("<impl>1.4</impl>") : "Version of impl not updated to 1.4"
