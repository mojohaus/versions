import java.util.regex.Pattern

def buf = new File(basedir, "child/pom.xml").readLines("UTF-8").collect { it + " " }.join('')
assert Pattern.compile("\\Q<parent>\\E.*\\Q<version>\\E\\s*5\\.0\\s*\\Q</version>\\E.*\\Q</parent>\\E").matcher(buf).find() : "Updated parent of child module"
buf = new File(basedir, "child/subchild/pom.xml").readLines("UTF-8").collect { it + " " }.join('')
assert Pattern.compile("\\Q<parent>\\E.*\\Q<version>\\E\\s*4\\.0\\s*\\Q</version>\\E.*\\Q</parent>\\E").matcher(buf).find() : "Updated parent of subchild module"
buf = new File(basedir, "child/subchild2/pom.xml").readLines("UTF-8").collect { it + " " }.join('')
assert Pattern.compile("\\Q<parent>\\E.*\\Q<version>\\E\\s*4\\.0\\s*\\Q</version>\\E.*\\Q</parent>\\E").matcher(buf).find() : "Updated parent of subchild2 module"
