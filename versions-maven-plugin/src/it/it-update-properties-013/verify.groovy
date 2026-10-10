def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert !buf.contains("<api>3.0</api>") : "Version updated to 3.0 when it shouldn't have been due to not being covered by the inclusion pattern"
