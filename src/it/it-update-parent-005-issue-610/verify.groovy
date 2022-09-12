pom = new File( basedir, "pom.xml" ).text

assert pom =~ /<version>999<\/version>/