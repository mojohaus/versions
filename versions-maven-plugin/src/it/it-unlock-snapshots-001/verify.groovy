def buf = new File(basedir, "pom.xml").getText("UTF-8")
assert buf.contains("<version>1.5.1-SNAPSHOT</version>") : "Version of plexus not unlocked to 1.5.1-SNAPSHOT"
