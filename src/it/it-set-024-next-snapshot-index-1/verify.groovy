pom = new File( basedir, "pom.xml" ).text

assert pom =~ /<version>2\.2\.3-SNAPSHOT<\/version>/
