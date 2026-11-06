def pom = new File(basedir, "pom.xml").text
def log = new File(basedir, "build.log").text
assert pom =~ /<artifactId>dummy-maven-plugin<\/artifactId>\s*<version>3.1<\/version>/
assert log.contains("Plugin updates are limited to versions that run on Maven 3.9.0: the project declares no minimum Maven version")
