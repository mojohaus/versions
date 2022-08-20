pom = new File( basedir, "pom.xml" ).text

assert pom =~ /<version>1\.3\.3-SNAPSHOT<\/version>/
