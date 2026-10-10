import java.util.regex.Pattern

def buf = new File(basedir, "pom.xml").readLines("UTF-8").collect { it + " " }.join('')
assert Pattern.compile("\\Q<parent>\\E.*\\Q<version>\\E\\s*3\\.0\\s*\\Q</version>\\E.*\\Q</parent>\\E").matcher(buf).find() : "Did not update parent to version 3.0"
