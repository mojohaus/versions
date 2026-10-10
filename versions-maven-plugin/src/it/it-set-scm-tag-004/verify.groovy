import java.util.regex.Pattern

def buf = new File(basedir, "pom.xml").readLines("UTF-8").collect { it + " " }.join('')
assert Pattern.compile("\\Q<scm>\\E\\s*\\Q<tag>\\Ev1\\.0\\Q</tag>\\E\\s*\\Q</scm>\\E").matcher(buf).find() : "Setting new tag"
