pom = new File( basedir, "pom.xml" ).text;

// expect outputTimestamp to be untouched
assert pom =~ /<project.build.outputTimestamp>10<.project.build.outputTimestamp>/
