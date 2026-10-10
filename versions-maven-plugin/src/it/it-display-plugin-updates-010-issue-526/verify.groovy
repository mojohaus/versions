def log = new File(basedir, "build.log").text
assert log.contains("No plugins require a newer version of Maven than specified by the pom.") : "Did not correctly detect minimum Maven build version 3.0 of this project"
assert log =~ /\Qlocalhost:dummy-maven-plugin\E\s*\.*\s*\Q1.0 -> 3.1\E/ : "Did not suggest updating dummy-maven-plugin to version 3.1"
