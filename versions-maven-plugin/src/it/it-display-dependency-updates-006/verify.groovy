import java.util.regex.Pattern

def buf = new File(basedir, "build.log").text
assert Pattern.compile("localhost:issue-114-artifact .* 1\\.0 -> 1\\.1").matcher(buf).find() : "localhost:issue-114-artifact 1.0 -> 1.1 was not processed."
assert Pattern.compile("localhost:dummy-api .* 2\\.0 -> 3\\.0").matcher(buf).find() : "localhost:dummy-api 2.0 -> 2.0 was not processed."
