def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert !buf.contains("<impl>2.2</impl>") : "Version updated for impl, when only API should have"
assert buf.contains("<api>3.0</api>") : "Version of api not updated to 3.0"
