import java.util.regex.Pattern

def buf = new File(basedir, "child/pom.xml").readLines("UTF-8").collect { it + " " }.join('')
assert Pattern.compile("\\Q<parent>\\E.*\\Q<version>\\E\\s*2\\.0\\s*\\Q</version>\\E.*\\Q</parent>\\E").matcher(buf).find() : "Updated parent of child module"
