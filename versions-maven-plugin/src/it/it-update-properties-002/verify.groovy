def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<api>1.3</api>") : "Version not updated to 1.3"
