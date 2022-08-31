pom = new File( basedir, "pom.xml" ).text;

assert pom =~ /<tag>v1\.0<\/tag>/
assert pom =~ /<connection>connection<\/connection>/
assert pom =~ /<developerConnection>developerConnection<\/developerConnection>/
assert pom =~ /<url>url<\/url>/