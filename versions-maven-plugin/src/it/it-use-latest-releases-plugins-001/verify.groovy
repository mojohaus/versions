def pom = new File(basedir, "pom.xml").text
def log = new File(basedir, "build.log").text
assert pom =~ /<artifactId>dummy-maven-plugin<\/artifactId>\s*<version>3.0<\/version>/
assert log.contains("Not updating localhost:dummy-maven-plugin:maven-plugin:1.0 to 3.1: it requires Maven 3.8.4")
