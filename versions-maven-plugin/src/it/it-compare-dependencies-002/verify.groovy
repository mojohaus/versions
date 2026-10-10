def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<junit.version>4.1</junit.version>") : "junit.version property was not updated"
assert buf.contains("<another.property>1</another.property>") : "another.property should not have changed"
