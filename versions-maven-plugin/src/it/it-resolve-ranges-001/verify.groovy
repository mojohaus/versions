def pom = new File(basedir, "pom.xml").getText("UTF-8")
assert pom.contains("<version>1.4</version>") : "Range [1.1, 2.0) of dummy-impl not resolved to 1.4"
assert pom.contains("<version>1.1.3</version>") : "Range [1.1.2, 1.1.3 ] of dummy-api not resolved to 1.1.3"
