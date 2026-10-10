import java.util.regex.Pattern

def buf = new File(basedir, "build.log").text
assert Pattern.compile("\\QIgnoring dependency with no version: localhost:dummy-api:jar\\E").matcher(buf).find() : "cannot find skipping of dummy-api dependency"
