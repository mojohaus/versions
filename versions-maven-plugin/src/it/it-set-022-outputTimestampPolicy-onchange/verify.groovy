pom = new File( basedir, "pom.xml" ).text

assert pom =~ /<project.build.outputTimestamp>10<.project.build.outputTimestamp>/
