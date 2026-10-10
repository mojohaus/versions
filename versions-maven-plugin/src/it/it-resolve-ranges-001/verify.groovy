def pom = new File(basedir, "pom.xml").getText("UTF-8")
assert ["2.0.6", "2.0.7", "2.0.8"].any { pom.contains("<version>$it</version>") } : "Version of maven-project not resolved to 2.0.6, 2.0.7, or 2.0.8"
assert pom.contains("<version>1.5.15</version>") : "Version of plexus not resolved to 1.5.15"
