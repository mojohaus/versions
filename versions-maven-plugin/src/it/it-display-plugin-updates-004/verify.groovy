import java.util.regex.Pattern

def buf = new File(basedir, "build.log").text
assert !Pattern.compile("\\QAll plugins have a version specified.\\E").matcher(buf).find() : "Did not notice that clean plugin does not have a version specified"
