def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<api>2.0</api>") : "Version not updated to 2.0"
