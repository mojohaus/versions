def buf = new File(basedir, "target/site/plugin-updates-report.html").readLines("UTF-8").collect { it + " " }.join('')
def result = buf.toString().replaceAll( "<[^>]+>", " " ).replaceAll( "&[^;]+;", " " ).replaceAll( "\\s+", " " )
assert result.contains("localhost dummy-maven-plugin 1.0 3.1") : "Did not generate dependency summary correctly" + "\n" + "Result = \"" + result +"\""
assert result.contains("dummy-api 1.1 jar 1.1.0-2 1.1.3 1.3 3.0") : "Did not generate dependency summary correctly" + "\n" + "Result = \"" + result +"\""
assert result.contains("Group Id localhost Artifact Id dummy-maven-plugin Current Version 1.0 Newer versions 2.0 " + "2.1 3.0 3.1 Latest Major") : "Did not identify plugin next versions" + "\n" + "Result = \"" + result +"\""
assert result.contains("Group Id localhost Artifact Id dummy-api Current Version 1.1 Classifier Type jar Newer " + "versions 1.1.0-2 Latest Subincremental 1.1.1 1.1.1-2 1.1.2 1.1.3 Latest Incremental 1.2 " + "1.2.1 1.2.2 1.3 Latest Minor 2.0 2.1 3.0 Latest Major") : "Did not identify dependency next versions" + "\n" + "Result = \"" + result +"\""
