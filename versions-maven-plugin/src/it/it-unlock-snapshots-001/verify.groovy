def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<version>3.4.0-SNAPSHOT</version>") : "Version of dummy-api not unlocked to 3.4.0-SNAPSHOT"
