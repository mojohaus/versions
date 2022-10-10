pom = new File( basedir, "pom.xml" ).text

assert !( pom =~ /2.0.0-rc1/ )
