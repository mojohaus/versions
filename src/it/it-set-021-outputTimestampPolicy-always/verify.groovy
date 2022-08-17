pom = new File( basedir, "pom.xml" ).text

assert pom =~ /<project.build.outputTimestamp>\d\d\d\d+<.project.build.outputTimestamp>/
