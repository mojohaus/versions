def buf = new File(basedir, "target/site/plugin-updates-report.html").readLines("UTF-8").collect { it + " " }.join('')
def result = buf.toString().replaceAll( "<[^>]+>", " " ).replaceAll( "&[^;]+;", " " ).replaceAll( "\\s+", " " )
assert result.contains("localhost dummy-maven-plugin 1.0 3.1") : "Did not generate summary correctly" + "\n" + "Result = \"" + result +"\""
assert result.contains("Group Id localhost Artifact Id dummy-maven-plugin Current Version 1.0 Newer versions 2.0 " + "2.1 3.0 3.1 Latest Major") : "Did not identify plugin next versions" + "\n" + "Result = \"" + result +"\""
