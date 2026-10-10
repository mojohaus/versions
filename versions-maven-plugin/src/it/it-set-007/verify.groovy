import java.util.regex.Pattern

def buf = new File(basedir, "pom.xml").readLines("UTF-8").collect { it + " " }.join('')
assert Pattern.compile("\\Q<version>\\E\\s*1\\.2\\.0-SNAPSHOT\\s*\\Q</version>\\E").matcher(buf).find() : "Did modify project version in parent pom"
buf = new File(basedir, "module-a1/pom.xml").readLines("UTF-8").collect { it + " " }.join('')
assert Pattern.compile("\\Q<version>\\E\\s*2\\.0\\.7-SNAPSHOT\\s*\\Q</version>\\E").matcher(buf).find() : "Did update project version in module-a1/pom"
buf = new File(basedir, "module-a2/pom.xml").readLines("UTF-8").collect { it + " " }.join('')
assert Pattern.compile("\\Q<version>\\E\\s*1\\.0\\.3-SNAPSHOT\\s*\\Q</version>\\E").matcher(buf).find() : "Did update project version in module-a2/pom"
assert Pattern.compile("\\Q<dependency>\\E.*\\Q<version>\\E\\s*2\\.0\\.6\\s*\\Q</version>\\E.*\\Q</dependency>\\E").matcher(buf).find() : "Did not update dependency version in module-a2/pom"
