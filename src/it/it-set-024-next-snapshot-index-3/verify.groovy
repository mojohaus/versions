pom = new File( basedir, "pom.xml" ).text

assert pom =~ /<version>1\.2\.4-SNAPSHOT<\/version>/
