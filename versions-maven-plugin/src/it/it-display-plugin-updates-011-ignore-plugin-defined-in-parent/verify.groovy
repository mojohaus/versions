import java.util.regex.Pattern

def buf = new File(basedir, "build.log").text
assert !Pattern.compile("\\Qlocalhost:dummy-maven-plugin\\E\\s*\\.+").matcher(buf).find() : "dummy-maven-plugin should not be listed in output, version is defined in Parent POM"
