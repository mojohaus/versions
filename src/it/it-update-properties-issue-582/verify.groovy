pom = new File( basedir, "pom.xml" ).text;

assert pom =~ /<api>3.0<\/api>/
