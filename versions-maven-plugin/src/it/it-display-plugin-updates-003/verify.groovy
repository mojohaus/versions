import java.util.regex.Pattern

def buf = new File(basedir, "build.log").text
assert Pattern.compile("\\Qlocalhost:dummy-maven-plugin\\E\\s*\\.*\\s*1\\.0\\s+->\\s+3\\.0").matcher(buf).find() : "Did not suggest updating dummy-maven-plugin to version 3.0"
