pom = new File( basedir, "pom.xml" ).text

assert pom =~ /<version>71-SNAPSHOT<\/version>/