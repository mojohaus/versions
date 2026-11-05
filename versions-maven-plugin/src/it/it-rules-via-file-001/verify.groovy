def pom = new File(basedir, "pom.xml").getText("UTF-8")
assert pom.contains("<version>3.0.1-1</version>") : "The rule ignoring version-rules 3.0.1-1.1 was not applied"
