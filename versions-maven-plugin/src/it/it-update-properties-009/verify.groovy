def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<api>2.1</api>") : "Version has been changed"
