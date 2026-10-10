import java.util.regex.Pattern

def buf = new File(basedir, "build.log").text
assert !Pattern.compile("\\Qlocalhost:dummy-api\\E\\s*\\.*\\s*\\Q[1.1,5.0)\\E\\s+->\\s+").matcher(buf).find() : "Did suggest updating dummy-api from range [1.1,5.0)"
