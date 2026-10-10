def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert !buf.contains("<version>[1.2, 1.3)</version>") : "Version of dummy-api not resolved"
assert !buf.contains("<version>[1.3, 1.4)</version>") : "Version of dummy-impl not resolved"
