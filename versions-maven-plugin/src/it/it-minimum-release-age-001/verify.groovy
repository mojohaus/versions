def buf = new File(basedir, "pom.xml").getText("UTF-8")
// every release in the mock repository is younger than 36500 days, so only the exempt dependency is updated
assert buf =~ /(?s)<artifactId>dummy-api<\/artifactId>\s*<version>3\.0<\/version>/ : "dummy-api is exempt and bumped to 3.0"
assert buf =~ /(?s)<artifactId>dummy-impl<\/artifactId>\s*<version>1\.0<\/version>/ : "dummy-impl is left at 1.0"
